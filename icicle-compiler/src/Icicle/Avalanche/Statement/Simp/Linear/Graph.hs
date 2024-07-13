{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE PatternGuards      #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DoAndIfThenElse    #-}
{-# LANGUAGE RecursiveDo        #-}
{-# LANGUAGE RecordWildCards    #-}

-- Remove copy operations on arrays which don't have shared references.
module Icicle.Avalanche.Statement.Simp.Linear.Graph (
  Graph,
  empty,
  overwrite,
  delete,
  merge,
  search,
)

where


import qualified    Data.Set as Set
import qualified    Data.Map.Lazy as Map

import              Icicle.Internal.Pretty

import              P hiding (empty)

-- | Simple graph library for doing a search.
--   These are directed graphs, with a maps for
--   outgoing and incoming edges.
data Graph n =
  Graph {
    graphForwards :: Map.Map n (Set.Set n)
  , graphBackwards :: Map.Map n (Set.Set n)
  }
 deriving (Eq, Ord, Show)

instance Pretty n => Pretty (Graph n) where
  pretty (Graph g _) =
    vsep (pretty <$> (Map.toList (Map.map Set.toList g)))

empty :: Graph n
empty =
  Graph Map.empty Map.empty

overwrite :: Ord n => n -> n -> Graph n -> Graph n
overwrite from to (Graph forwards backwards) =
  let
    existing =
      Map.lookup from forwards

    removeBinds (Just xs) = do
      found <- Just $ Set.delete from xs
      guard (not (Set.null found))
      return found

    removeBinds Nothing =
      Nothing

    trimmedBackwards =
      case existing of
        Nothing ->
          backwards
        Just set ->
          Set.foldl' (\v k -> Map.alter removeBinds k v) backwards set
  in
  if (from == to) then
    Graph {
      graphForwards =
        Map.delete from forwards
    , graphBackwards =
         Map.alter removeBinds to trimmedBackwards
    }
  else
    Graph {
      graphForwards =
        Map.insert from (Set.singleton to) forwards
    , graphBackwards =
        Map.alter (Just . maybe (Set.singleton from) (Set.insert from)) to trimmedBackwards
    }

-- | Delete a node and stitch up its edges such that
--   nodes which were transitively connected through
--   the node are now directly connected.
delete :: Ord n => n -> Graph n -> Graph n
delete from (Graph forwards backwards) =
  let
    transientForwards =
      fromMaybe Set.empty $
        Map.lookup from forwards

    transientBackwards =
      fromMaybe Set.empty $
        Map.lookup from backwards

    addIfPoint transient k xs =
      Set.delete k $ Set.delete from $
        if Set.member from xs then
          Set.union transient xs
        else
          xs

    addIfPointOrNone transient k (Just xs) = do
      found <- Just $ addIfPoint transient k xs
      guard (not (Set.null found))
      return found

    addIfPointOrNone _ _ Nothing =
      Nothing

    stitchUp touch transient base =
      Set.foldl' (\v k -> Map.alter (addIfPointOrNone transient k) k v) base touch

  in
  Graph {
    graphForwards =
      stitchUp transientBackwards transientForwards (Map.delete from forwards)
  , graphBackwards =
      stitchUp transientForwards transientBackwards (Map.delete from backwards)
  }

match :: Ord n => n -> Map.Map n (Set.Set n) -> (Set.Set n, Map.Map n (Set.Set n))
match n g =
  case Map.lookup n g of
    Nothing -> (Set.empty, g)
    Just set -> (set, Map.delete n g)

search :: Ord n => Set.Set n -> Graph n -> Set.Set n
search ns (Graph g _) = searchInternal ns g

searchInternal :: Ord n => Set.Set n -> Map.Map n (Set.Set n) -> Set.Set n
searchInternal ns _ | Set.null ns = Set.empty
searchInternal ns g =
  let go (s', g') n'   = let (found', remains') = match n' g' in (Set.union s' found', remains')
      (found, remains) = foldl' go (Set.empty, g) ns
  in Set.union found (searchInternal found remains)

merge :: Ord n => Graph n -> Graph n -> Graph n
merge (Graph a x) (Graph b y) =
  Graph {
    graphForwards =
      Map.unionWith Set.union a b
  , graphBackwards =
      Map.unionWith Set.union x y
  }
