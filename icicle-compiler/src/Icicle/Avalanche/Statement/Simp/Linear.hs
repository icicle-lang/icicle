{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE PatternGuards      #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE FlexibleContexts   #-}

-- Remove copies of arrays which don't have shared references.
module Icicle.Avalanche.Statement.Simp.Linear (
    linearise
  ) where

import qualified    Data.Graph.Inductive as Graph
import qualified    Data.Set as Set
import              Data.Hashable (Hashable (..))

import              Icicle.Avalanche.Statement.Statement
import              Icicle.Common.Exp
import qualified    Icicle.Avalanche.Prim.Flat as Flat

import              P

-- | Minimise clones of arrays.
--
--   This pass is currently pretty naïve, and is only really designed to make
--   sure that `group` expressions don't create a new array for their maps
--   with every new data point added to the map.
--
--   The idea is that if arrays are exclusively accessed in an affine manner,
--   then we don't need to clone them, and can instead act on them destructively.
--
--   To do this, we build a graph data structure of all bindings (this should
--   be specialised to accumulators separately to bindings probably) which
--   _might_ reference the same array in memory. It's ok to be conservative.
--
--   Then, we build a usage set where we track what variables are used later
--   in the program, and propagate that information backwards to our writes.
--
--   Finally, if we encounter a write which just writes a copy of an array, and
--   no values which might share a reference are used after this, then we
--   delete the copy, and just use the original array.
--
--   The tricky parts here are writes in blocks or conditions, some
--   write information propagates both forwards and backwards.
--
linearise :: (Hashable n, Eq n) => Statement a n Flat.Prim -> Statement a n Flat.Prim
linearise =
  third <$> go emptyGraph Set.empty
    where

  emptyGraph :: Graph.Gr () ()
  emptyGraph =
    Graph.empty

  third (_,_,c) =
    c


  -- Delete copy operations if they don't change the result
  -- after C elaboration.
  --
  -- Arguments:
  -- · Graph of variables and where thing might point to in scope
  -- · Variables which are used in future statements
  -- · The statement
  --
  -- Returns
  -- · Variables used by this statement and ones in the future
  -- · Graph of variables which were written to
  -- · The new statement
  --
  go aliased used statements =
    case statements of
      --
      -- If it nominally pretty simple, but we want to ensure
      -- that we run in linear time, so we don't want to join
      -- the results. This means that we run one of the sides
      -- twice, passing back the used map through it from the
      -- other branch.
      If x ts fs
        -> let (tU, tA, tS) = go aliased used ts
               (_,   _, tF) = go aliased used fs
               (fU, gA,  _) = go tA tU fs
            in (freevars x <> fU, gA, If x tS tF)

      -- When we read an accumulator to a value, we insert into
      -- the graph the link between them.
      -- It would probably be best to remove the name as well
      -- from the return, but because we're using the Hash as
      -- the key, we might accidentally delete a collision.
      Read nx na t ss
        -> let (sU, aC, sS) = go (insert nx na aliased) used ss
            in (sU, aC, Read nx na t sS)

      -- If the binding looks like it might reference, then add
      -- it to the graph. The usage will appear in freevars too.
      Let n x ss
        | Just na <- arrayReference x
        -> let (sU, aC, sS) = go (insert n na aliased) used ss
            in (freevars x <> sU, aC, Let n x sS)

      Let n x ss
        -> let (sU, aC, sS) = go aliased used ss
            in (freevars x <> sU, aC, Let n x sS)

      InitAccumulator acc@(Accumulator nm _ x) ss
        | Just ref <- arrayReference x
        -> let (sU, aC, sS) = go (insert nm ref aliased) used ss
            in (freevars x <> sU, aC, InitAccumulator acc sS)

      InitAccumulator acc@(Accumulator _ _ x) ss
        -> let (sU, aC, sS) = go aliased used ss
            in (freevars x <> sU, aC, InitAccumulator acc sS)

      -- Here's the key judgement and rewrite.
      -- This write just copies some array into an accumulator.
      -- If the used set of accumulators and names from future
      -- statements does not include anything which comes from
      -- the array we are copying, then we don't need to actually
      -- copy it and can instead use it directly.
      Write n x
        | Just (Flat.PrimArray (Flat.PrimArrayCopy _), [XVar a nm]) <- takePrimApps x
        , let bound = insert n nm aliased
        , isUnused n bound used
        -> (freevars x <> used, bound, Write n (XVar a nm))

      Write n x
        | Just ref <- arrayReference x
        -> (freevars x <> used, insert n ref aliased, Write n x)

      Write n x
        -> (freevars x <> used, aliased, Write n x)

      -- Blocks are where things get interesting.
      -- We need to traverse _backwards_ to gather usage information.
      -- But we need to make sure that aliases push information forwards
      -- where they need to
      Block []
        -> (mempty, aliased, statements)

      -- Only writes can propagate aliases backwards and forwards.
      -- Here, we're accounting for the write pushing forwards by writing
      -- to an accumulator in scope.
      -- Init, Let, and Read all introduce scope, so don't need to be
      -- handled here.
      Block (t@(Write n x):rs)
        | Just ref <- arrayReference x
        -> let (rU, aC, rS) = go (insert n ref aliased) used (Block rs)
               (tU, aR, tS) = go aC rU t
           in case rS of
             Block rS' -> (tU, aR, Block (tS : rS'))
             _         -> (tU, aR, Block [tS, rS])

      -- Anything which isn't a write just do the backwards pass.
      Block (t:rs)
        -> let (rU, aC, rS) = go aliased used (Block rs)
               (tU, aR, tS) = go aC rU t
           in case rS of
             Block rS' -> (tU, aR, Block (tS : rS'))
             _         -> (tU, aR, Block [tS, rS])

      -- Loops.
      -- Nothing special is required, the graph will itself find
      -- all loop conditions which span across multiple iterations.
      While a w vt x ss
        -> let (rU, aR, sS) = go aliased (freevars x <> used) ss
           in (rU, aR, While a w vt x sS)

      ForeachInts t n from to ss
        -> let (rU, aR, sS) = go aliased used ss
           in (rU, aR, ForeachInts t n from to sS)

      ForeachFacts binds vt ss
        -> let (rU, aR, sS) = go aliased used ss
           in (rU, aR, ForeachFacts binds vt sS)

      Output n t xts
        -> let xsU = Set.unions (fmap (freevars . fst) xts)
           in  (xsU <> used, aliased, Output n t xts)

    where
      -- Has a different binding been used which might point
      -- to the same underlying memory location as this one?
      isUnused nx as us =
        let
          theseAliases =
            Set.fromList $
              Graph.dfs [hash nx] as

          thoseAliases =
            let
              otherNamesUsed =
                Set.delete nx us
            in
            Set.fromList $
              Graph.dfs (hash <$> Set.toList otherNamesUsed) as

         in
         Set.disjoint theseAliases thoseAliases

      addIfUnseen n acc =
        if Graph.gelem (hash n) acc then
          acc
        else
          Graph.insNode (hash n, ()) acc

      insert nx na as =
        let
          as' =
            addIfUnseen nx as
          as'' =
            addIfUnseen na as'
        in
        Graph.insEdge (hash nx, hash na, ()) as''

      arrayReference x =
        case x of
          XVar _ nm ->
            Just nm

          _ | Just (Flat.PrimArray (Flat.PrimArrayPutMutable _), [g,_,_]) <- takePrimApps x ->
            arrayReference g

          _ | Just (Flat.PrimArray (Flat.PrimArraySwap _), [g,_,_]) <- takePrimApps x ->
            arrayReference g

          _ | Just (Flat.PrimUnsafe (Flat.PrimUnsafeArrayIndex _), [g,_]) <- takePrimApps x ->
            arrayReference g

          _ | Just (Flat.PrimBuf (Flat.PrimBufRead {}), [g]) <- takePrimApps x ->
            arrayReference g

          _ ->
            Nothing
