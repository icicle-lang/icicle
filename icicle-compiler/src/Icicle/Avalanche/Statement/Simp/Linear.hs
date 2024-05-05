{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE PatternGuards      #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DoAndIfThenElse    #-}
{-# LANGUAGE RecursiveDo        #-}

-- Remove copy operations on arrays which don't have shared references.
module Icicle.Avalanche.Statement.Simp.Linear (linearise) where

import              Control.Monad.Tardis

import qualified    Data.Set as Set
import              Data.Hashable (Hashable (..))
import qualified    Data.Map.Lazy as Map

import qualified    Icicle.Avalanche.Prim.Flat as Flat
import              Icicle.Avalanche.Statement.Statement
import              Icicle.Common.Base (Name)
import              Icicle.Common.Exp
import              Icicle.Common.Type (ValType (..))

import              P hiding (empty)

-- | Minimise clones of arrays.
--
--   This pass is currently pretty naïve, and is only really designed to make
--   sure that `group` expressions don't create a new array for their maps
--   with every new data point added to the map.
--
--   The idea is that if arrays are exclusively accessed in an affine manner,
--   then we don't need to clone them, and can instead act on them destructively.
--
--   To do this, we build a graph data structure of all bindings which might
--   reference the same memory location in a future part of the program.
--
--   We also build a usage set where we track what variables are used later
--   in the program, and propagate that information backwards.
--
--   Finally, if we encounter a write which just writes a copy of an array, and
--   no values which might share a reference are used after this, then we
--   delete the copy, and just use the original array.
--
--   The tricky part here is that information travels in both directions, so we
--   need to not blow out our runtime.
--
linearise :: (Hashable n, Eq n) => Statement a n Flat.Prim -> Statement a n Flat.Prim
linearise s =
  evalTardis (go s) (Set.empty, empty)
    where

  --
  -- Delete copy operations if they won't change the result when elaborated into C.
  --
  -- The Tardis Monad is used to propagate usage information backwards and the alias map
  -- forwards.
  --
  -- It's likely possible this can be done without the Tardis Monad and just with laziness,
  -- but it seems to make the algorithm simpler, especially for Blocks.
  go statements =
    case statements of
      --
      -- If blocks are tricky, in that only one will run, so individually shouldn't see
      -- what the other branch does. Before and after though, we need to accommodate either
      -- side running. So we break out of the monad, and then merge the results to send each
      -- way.
      If x ts fs -> mdo
        modifyBackwardsUsed (tU <> fU <> freevars x)

        aliased            <- getPast
        let ~(tS, (tU, tA)) = runTardis (go ts) (used, aliased)
        let ~(fS, (fU, fA)) = runTardis (go fs) (used, aliased)
        used               <- getFuture

        modifyForwards $
          merge tA . merge fA

        return $
          If x tS fS

      --
      -- Reads introduce scope for future consideration.
      -- If it's an array, we say that it aliases the one it points to in the future.
      Read nx na t@(ArrayT {}) ss -> do
        modifyBackwardsUsedRemoveBind nx na
        -- Modify forwards before entering the block
        modifyForwards $
          overwrite nx na

        Read nx na t <$> go ss

      --
      -- It it's not an array, we don't mind and don't declare the alias.
      Read nx na t ss -> do
        modifyBackwardsUsedRemoveBind nx na
        Read nx na t <$> go ss

      --
      -- Let bindings are a lot like reads, if it looks like a reference, then add
      -- the alias.
      Let nm x ss | Just ref <- arrayReference x -> do
        modifyBackwardsUsedRemoveBind' nm x
        -- Modify forwards before entering the block
        modifyForwards $
          overwrite nm ref

        Let nm x <$> go ss

      Let nm x ss -> do
        modifyBackwardsUsedRemoveBind' nm x
        Let nm x <$> go ss

      --
      -- Initialisation is similar again to lets and reads.
      InitAccumulator acc@(Accumulator nm (ArrayT {}) x) ss | Just ref <- arrayReference x -> do
        modifyBackwardsUsedRemoveBind' nm x
        -- Modify forwards before entering the block
        modifyForwards $
          overwrite nm ref

        InitAccumulator acc <$> go ss

      InitAccumulator acc@(Accumulator nm _ x) ss -> do
        modifyBackwardsUsedRemoveBind' nm x
        InitAccumulator acc <$> go ss

      --
      -- Here's the key judgement and rewrite.
      -- This write just copies some array into an accumulator.
      -- If the used set of accumulators and names from future
      -- statements does not include anything which comes from
      -- the array we are copying, then we don't need to actually
      -- copy it and can instead use it directly.
      Write n x | Just (Flat.PrimArray (Flat.PrimArrayCopy _), [XVar a ref]) <- takePrimApps x -> do
        modifyBackwardsUsed' x
        aliased <- getPast
        used    <- getFuture

        -- Do we need to add a new alias for this? If there are no
        -- further references then we don't delete the copy, then
        -- there technically should be an alias, but, on the other
        -- hand, it doesn't matter, because there are no aliases which
        -- refer to anything before this address or this address so no
        -- path can be formed; on the other hand, if we do keep the
        -- copy, then there's no alias required, because it's a fresh
        -- memory location. Either way, we need to make the same
        -- decision, because if we change the sent signals here based
        -- on them, we won't be able to fix the Tardis.

        pure $
          if hasNoFurtherReferences n ref aliased used then do
            Write n (XVar a ref)
          else do
            Write n x

      --
      -- Otherwise, if the write is a reference, then we need
      -- to know that this memory location points to the old
      -- one in subsequent items in a block.
      Write n x | Just ref <- arrayReference x -> do
        modifyBackwardsUsed' x
        modifyForwards $
          insert n ref

        pure $ Write n x

      Write n x -> do
        modifyBackwardsUsed' x
        pure $ Write n x

      --
      -- Traverse the block items in order; the future and past
      -- states will sort themselves out.
      Block xs ->
        Block <$> traverse go xs

      While a w vt x ss -> do
        modifyBackwardsUsed' x
        While a w vt x <$> go ss

      ForeachInts t n from to ss -> do
        modifyBackwardsUsed' from
        modifyBackwardsUsed' to
        ForeachInts t n from to <$> go ss

      ForeachFacts binds vt ss -> do
        ForeachFacts binds vt <$> go ss

      x@(Output _ _ xts) -> do
        for_ xts $
          modifyBackwardsUsed' . fst

        pure x

    where
      --
      -- Can we remove the bindings for lets and reads?
      -- If we make the assumption that any refs which the binding `n` is made from must
      -- be derived from the expression `x`, then yes. We're effectively substituting any
      -- (optional) usages of the downstream named bindings with the usage to create it.
      -- This should help keep our backwards `Set` small, which is important because we
      -- need to do a `union` step for our `If` statements, which is effectively `O(N)`
      -- there, as the sizes of the sets are similar.
      modifyBackwardsUsedRemoveBind' n x =
        modifyBackwards (Set.delete n . Set.union (freevars x))
      modifyBackwardsUsedRemoveBind n acc =
        modifyBackwards (Set.delete n . Set.insert acc)

      modifyBackwardsUsed =
        modifyBackwards . Set.union
      modifyBackwardsUsed' =
        modifyBackwardsUsed . freevars


hasNoFurtherReferences :: Ord a => a -> a -> Graph a -> Set.Set a -> Bool
hasNoFurtherReferences acc nx aliased used =
  let
    theseAliases =
      Set.insert nx (search [nx] aliased)

    thoseAliases =
      search (Set.toList (Set.delete acc used)) aliased
  in
    Set.disjoint
      theseAliases
      thoseAliases


arrayReference :: Exp a n Flat.Prim -> Maybe (Name n)
arrayReference x =
  case x of
    XVar _ nm ->
      Just nm

    _ | Just (Flat.PrimArray (Flat.PrimArrayPutMutable _), [g,_,_]) <- takePrimApps x ->
      arrayReference g

    _ | Just (Flat.PrimArray (Flat.PrimArraySwap _), [g,_,_]) <- takePrimApps x ->
      arrayReference g

    _ | Just (Flat.PrimArray (Flat.PrimArrayDel _), [g,_]) <- takePrimApps x ->
      arrayReference g

    --
    -- This one's different. Nested arrays in arrays are a bit tricky, especially if we're going
    -- to consider something like a Heap Sort. Instead, just say all values in a nested array
    -- share a reference with the top array.
    _ | Just (Flat.PrimUnsafe (Flat.PrimUnsafeArrayIndex (ArrayT {})), [g,_]) <- takePrimApps x ->
      arrayReference g

    _ ->
      Nothing

--
-- | Dead simple graph library for doing a search.
newtype Graph n =
  Graph (Map.Map n (Set.Set n))
 deriving (Eq, Ord, Show)

empty :: Graph n
empty =
  Graph Map.empty

insert :: Ord n => n -> n -> Graph n -> Graph n
insert from to (Graph g) =
  Graph $
    Map.alter (Just . maybe (Set.singleton to) (Set.insert to)) from g

overwrite :: Ord n => n -> n -> Graph n -> Graph n
overwrite from to (Graph g) =
  Graph $
    Map.insert from (Set.singleton to) g

match :: Ord n => n -> Graph n -> (Set.Set n, Graph n)
match n (Graph g) =
  case Map.lookup n g of
    Nothing -> (Set.empty, Graph g)
    Just set -> (set, Graph $ Map.delete n g)

search :: Ord n => [n] -> Graph n -> Set.Set n
search [] _ = Set.empty
search ns g =
  let go (s', g') n'   = let (found', remains') = match n' g' in (Set.union s' found', remains')
      (found, remains) = foldl' go (Set.empty, g) ns
  in Set.union found (search (Set.toList found) remains)

merge :: Ord n => Graph n -> Graph n -> Graph n
merge (Graph a) (Graph b) =
  Graph $
    Map.unionWith Set.union a b
