{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Icicle.Test.Avalanche.Simp.Linear.Graph where

import           P
import qualified Data.Set as Set
import           System.IO
import           Hedgehog
import qualified Hedgehog.Gen as Gen

import qualified Icicle.Avalanche.Statement.Simp.Linear.Graph as Graph
import qualified Hedgehog.Internal.Range as Range


prop_tidies_reverse :: Property
prop_tidies_reverse = withTests 1 . property $ do
  let
    combined =
      Graph.insert 'a' 'b' $
      Graph.insert 'b' 'c' $
      Graph.insert 'b' 'd' Graph.empty

    expected =
      Graph.insert 'a' 'c' $
      Graph.insert 'a' 'd' Graph.empty

  Graph.delete 'b' combined === expected


prop_tidies_reverse_source :: Property
prop_tidies_reverse_source = withTests 1 . property $ do
  let
    combined =
      Graph.insert 'a' 'b' $
      Graph.insert 'b' 'c' $
      Graph.insert 'x' 'b' Graph.empty

    expected =
      Graph.insert 'a' 'c' $
      Graph.insert 'x' 'c' Graph.empty

  Graph.delete 'b' combined === expected


prop_cross_products :: Property
prop_cross_products = withTests 1 . property $ do
  let
    combined =
      Graph.insert 'a' 'x' $
      Graph.insert 'b' 'x' $
      Graph.insert 'x' 'c' $
      Graph.insert 'x' 'd' Graph.empty

    expected =
      Graph.insert 'a' 'c' $
      Graph.insert 'a' 'd' $
      Graph.insert 'b' 'c' $
      Graph.insert 'b' 'd' Graph.empty

  Graph.delete 'x' combined === expected


prop_add_remove_source :: Property
prop_add_remove_source = withTests 1 . property $ do
  let
    trashed =
      Graph.delete 'a' $
        Graph.overwrite 'a' 'b' Graph.empty

  trashed === Graph.empty

prop_add_remove_dest :: Property
prop_add_remove_dest = withTests 1 . property $ do
  let
    trashed =
      Graph.delete 'b' $
        Graph.overwrite 'a' 'b' Graph.empty

  trashed === Graph.empty


prop_overwrite_removes_dangling :: Property
prop_overwrite_removes_dangling = withTests 1 . property $ do
  let
    trashed =
      Graph.overwrite 'a' 'c' $
        Graph.overwrite 'a' 'b' Graph.empty

  trashed === Graph.overwrite 'a' 'c' Graph.empty


prop_overwrite_removes_evidence :: Property
prop_overwrite_removes_evidence = property $ do
  xs     <- forAll $ Gen.list (Range.constant 0 20) $ (,) <$> Gen.digit <*> Gen.digit
  start  <- forAll Gen.digit
  endOne <- forAll Gen.digit
  endTwo <- forAll Gen.digit

  let
    initial =
      foldr (uncurry Graph.insert) Graph.empty xs
    toBlat =
      Graph.overwrite start endTwo initial

  Graph.overwrite start endOne toBlat === Graph.overwrite start endOne initial


prop_search_finds_obvious :: Property
prop_search_finds_obvious = withTests 1 . property $ do
  let
    combined =
      Graph.insert 'a' 'x' $
      Graph.insert 'b' 'x' $
      Graph.insert 'x' 'c' $
      Graph.insert 'x' 'd' Graph.empty

  Graph.search (Set.singleton 'a') combined === Set.fromList "axcd"


tests :: IO Bool
tests =
  checkParallel $$(discover)
