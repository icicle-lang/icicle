{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Icicle.Test.Avalanche.Simp.Linear.Graph where

import           P
import           System.IO
import           Hedgehog
import qualified Hedgehog.Gen as Gen

import qualified Icicle.Avalanche.Statement.Simp.Linear.Graph as Graph
import qualified Hedgehog.Internal.Range as Range


prop_tidies_reverse :: Property
prop_tidies_reverse = withTests 1 . property $ do
  let
    a2b2c =
      Graph.overwrite 'a' 'b' $
        Graph.overwrite 'b' 'c' Graph.empty
    a2b2d =
      Graph.overwrite 'a' 'b' $
        Graph.overwrite 'b' 'd' Graph.empty
    combined =
      Graph.merge a2b2c a2b2d

    trashed =
      Graph.delete 'b' combined

    recreated =
      Graph.merge
        (Graph.overwrite 'a' 'c' Graph.empty)
        (Graph.overwrite 'a' 'd' Graph.empty)

  trashed === recreated

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

prop_overwrite_removes_evidence :: Property
prop_overwrite_removes_evidence = property $ do
  xs     <- forAll $ Gen.list (Range.constant 0 20) $ (,) <$> Gen.digit <*> Gen.digit
  start  <- forAll Gen.digit
  endOne <- forAll Gen.digit
  endTwo <- forAll Gen.digit

  let
    initial =
      foldr (uncurry Graph.overwrite) Graph.empty xs
    toBlat =
      Graph.overwrite start endTwo initial

  annotateShow toBlat

  Graph.overwrite start endOne toBlat === Graph.overwrite start endOne initial


tests :: IO Bool
tests =
  checkParallel $$(discover)
