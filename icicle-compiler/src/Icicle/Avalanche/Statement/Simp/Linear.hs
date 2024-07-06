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
import              Icicle.Internal.Pretty

import              P hiding (empty)

-- | Minimise clones of arrays.
--
--   Icicle is a pure language, but when it gets to C, its maps and arrays
--   are actually just C arrays.
--
--   This means that map insert, and array sort functions have to copy the
--   whole array to maintain pure semantics; but there are a lot of cases
--   when this copy won't change the program results and can be removed.
--
--   The idea here is that when an array is exclusively accessed in an
--   affine manner, then we don't actually need to clone it, and can
--   instead act on them destructively.
--
--   This pass performs copy elision when an array is provably not accessed
--   by a binding (which is really a pointer to the heap) which is not the
--   result of the copy or an alias of said copy – that is, there can't be any
--   usage of the name of the binding being copied or any other previously
--   known reference.
--
--   To to this, in a forwards pass we build a graph of aliases to arrays in
--   scope; and, lazily in a backwards pass, we build a usage map of all bindings
--   in scope.
--
--   Finally, if we encounter a write which just writes a copy of an array, and
--   no values which might share a reference are used after this, then we
--   delete the copy, and just use the original array.
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
        Read nx na t <$>
          scopedGo nx na ss


      --
      -- It it's not an array, we don't mind and don't declare the alias.
      Read nx na t ss -> do
        modifyBackwardsUsedRemoveBind nx na
        Read nx na t <$>
          scopedGo' nx ss

      --
      -- Let bindings are a lot like reads, if it looks like a reference, then add
      -- the alias.
      Let nm x ss | Just ref <- arrayReference x -> do
        modifyBackwardsUsedRemoveBind' nm x
        Let nm x <$>
          scopedGo nm ref ss

      Let nm x ss -> do
        modifyBackwardsUsedRemoveBind' nm x
        Let nm x <$>
          scopedGo' nm ss

      --
      -- Initialisation is similar again to lets and reads.
      InitAccumulator acc@(Accumulator nm (ArrayT {}) x) ss | Just ref <- arrayReference x -> do
        modifyBackwardsUsedRemoveBind' nm x
        InitAccumulator acc <$>
          scopedGo nm ref ss


      InitAccumulator acc@(Accumulator nm _ x) ss -> do
        modifyBackwardsUsedRemoveBind' nm x
        InitAccumulator acc <$>
          scopedGo' nm ss

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

        --
        -- Do we need to add a new alias for this?
        -- Sequentially, it shouldn't matter to not include this, but
        -- for loops it's important, as we need to check what aliases
        -- are possible in future iterations.
        -- We can't put this in the if condition, because if we change
        -- the subsequent calculations depending on usage from the
        -- future, we won't fix the Tardis.

        modifyForwards $
          overwrite n ref

        pure $
          if hasNoFurtherReferences n ref aliased used then do
            Write n (XVar a ref)
          else do
            Write n x

      --
      -- If the write is a reference, then we need
      -- to know that this memory location points to the old
      -- one in subsequent items in a block.
      Write n x | Just ref <- arrayReference x -> do
        modifyBackwardsUsed' x
        modifyForwards $
          overwrite n ref

        pure $ Write n x

      Write n x -> do
        modifyBackwardsUsed' x
        pure $ Write n x

      --
      -- Traverse the block items in order; the future and past
      -- states will sort themselves out.
      Block xs ->
        Block <$> traverse go xs

      --
      -- Looping constructs.
      -- Run until a fixpoint is reached.
      While a w vt x ss -> do
        modifyBackwardsUsed' x
        While a w vt x <$>
          fixGo ss

      ForeachInts t n from to ss -> do
        modifyBackwardsUsed' from
        modifyBackwardsUsed' to
        ForeachInts t n from to <$>
          fixGo ss

      ForeachFacts binds vt ss -> do
        ForeachFacts binds vt <$>
          fixGo ss

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

      --
      -- This introduces the alias for name to ref, then when it goes
      -- out of scope, deletes this name and remakes transient aliases
      -- as direct ones.
      scopedGo nm ref ss = do
        modifyForwards (overwrite nm ref)
        sS <- go ss
        modifyForwards (delete nm)
        pure sS

      --
      -- This doesn't actually introduce an alias into the map,
      -- but if this is something like array_create, then we want
      -- to remove the reference after it goes out of scope, so that
      -- the new value stands alone.
      scopedGo' nm ss = do
        sS <- go ss
        modifyForwards (delete nm)
        pure sS

      --
      -- Loops are a bit tricky.
      -- It's possible to write queries where after a pass is complete
      -- accumulators depend on other accumulators initialised before
      -- the loop began; so we need to reach a fixpoint on both the
      -- initial alias map and the returned alias map.
      --
      -- If the maps don't match, rerun with the merged alias map on
      -- the original statements. We need to use the original ones
      -- because the pass might have deleted some copy operations which
      -- are actually required given the new information.
      fixGo ss = do
        before <- getPast
        sS     <- go ss
        after  <- getPast
        let merged = merge before after
        if before == merged then
          pure sS
        else do
          sendFuture merged
          fixGo ss


hasNoFurtherReferences :: Ord a => a -> a -> Graph a -> Set.Set a -> Bool
hasNoFurtherReferences acc nx aliased used =
  let
    theseAliases =
      Set.insert nx (search (Set.singleton nx) aliased)

    thoseAliases =
      search (Set.delete acc used) aliased

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

instance Pretty n => Pretty (Graph n) where
  pretty (Graph g) =
    vsep (pretty <$> (Map.toList (Map.map Set.toList g)))

empty :: Graph n
empty =
  Graph Map.empty

overwrite :: Ord n => n -> n -> Graph n -> Graph n
overwrite from to (Graph g) =
  Graph $
    Map.insert from (Set.singleton to) g

delete :: Ord n => n -> Graph n -> Graph n
delete from (Graph g) =
  let
    transient =
      fromMaybe Set.empty $
        Map.lookup from g

    addIfPoint k xs =
      Set.delete k $ Set.delete from $
        if Set.member from xs then
          Set.union transient xs
        else
          xs

    addIfPointOrNone k xs = do
      found <- Just $ addIfPoint k xs
      guard (not (Set.null found))
      return found

  in
  Graph $
    Map.mapMaybeWithKey addIfPointOrNone $
      Map.delete from g

match :: Ord n => n -> Graph n -> (Set.Set n, Graph n)
match n (Graph g) =
  case Map.lookup n g of
    Nothing -> (Set.empty, Graph g)
    Just set -> (set, Graph $ Map.delete n g)

search :: Ord n => Set.Set n -> Graph n -> Set.Set n
search ns _ | Set.null ns = Set.empty
search ns g =
  let go (s', g') n'   = let (found', remains') = match n' g' in (Set.union s' found', remains')
      (found, remains) = foldl' go (Set.empty, g) ns
  in Set.union found (search found remains)

merge :: Ord n => Graph n -> Graph n -> Graph n
merge (Graph a) (Graph b) =
  Graph $
    Map.unionWith Set.union a b
