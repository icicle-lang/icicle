{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sorbet.PrettyParse where

import           Icicle.Test.Arbitrary
import qualified Icicle.Test.Gen.Core.Type as CoreGen

import           P

import qualified Data.Text as Text
import           Data.String

import           System.IO

import           Hedgehog hiding (Var)
import qualified Hedgehog.Gen.QuickCheck as Gen

import           Icicle.Common.Base
import           Icicle.Internal.Pretty

import           Icicle.Sorbet.Parse
import           Icicle.Sorbet.Render

import           Icicle.Source.Query as Query
import           Icicle.Source.Type  as Type
import           Icicle.Source.Transform.Base

prop_parse_pretty_same :: Property
prop_parse_pretty_same = withDiscards 1000 . withTests 1000 . property $ do
  -- Ill-typed programs will be printed/parsed incorrectly if they have operators
  -- with the wrong number of arguments - for example (!) applied to no arguments.
  -- However, by setting the tableflip ratio to 0,
  -- it will generate only the correct number of arguments
  qwf <- forAll $ Gen.quickcheck (genQueryWithFeatureTypedGen 0)

  let
    q  = qwfQueryTop qwf
    pp = show $ pretty (PrettySorbet q)
    t  = Text.pack pp

    parsed  = sorbet (queryName q) t
    parsed' = second (reannot (const ())) parsed

  when ("before" `Text.isInfixOf` t) discard
  when ("between" `Text.isInfixOf` t) discard
  when ("after" `Text.isInfixOf` t) discard
  when ("days" `Text.isInfixOf` t) discard
  when ("seconds" `Text.isInfixOf` t) discard
  when ("months" `Text.isInfixOf` t) discard
  when ("years" `Text.isInfixOf` t) discard

  let
    name builtin
      = nameOf . NameBase . fromString . show . pretty $ builtin

    replacePrim () x
      | Prim a (Fun p) <- x
      , p `elem` listOfIntroducedFuns
      = return ((), Query.Var a (name p))
      | otherwise
      = return ((), x)

    -- Replace introduced primitives in the generated
    -- query with vars with the same name, as we will
    -- actually inline them to the primitive at a later
    -- phase.
    rewriter
      = idTransform { transformExp = replacePrim }

    normalised =
      transformQT rewriter q

  Hedgehog.annotate pp
  Hedgehog.annotate $
    case parsed of
      Left e   -> show $ pretty e
      Right q' -> show $ pretty q'

  parsed' === normalised


prop_parse_pretty_type :: Property
prop_parse_pretty_type = withDiscards 1000 . withTests 1000 . property $ do
  valType   <- forAll CoreGen.genValType
  let
    sourceType =
      Forall [] [] (Type.typeOfValType valType)

  tripping sourceType (Text.pack . show . pretty)
      sorbetType


return []
tests :: IO Bool
tests = checkParallel $$(discover)
