{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sorbet.Lexical.Lexer where

import           Icicle.Sorbet.Lexical.Lexer
import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           Icicle.Test.Sorbet.Lexical.Gen

import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           System.IO (IO)

import qualified Text.Megaparsec as Mega


prop_lexer_roundtrip :: Property
prop_lexer_roundtrip =
  property $ do
    tokens <- forAll (Gen.list (Range.linear 1 100) jToken)
    let
      render =
        T.unwords . fmap renderToken

      parse =
        fmap (fmap posTail) .
          Mega.parse lexProgram "qc.icicle"

    tripping tokens render parse

tests :: IO Bool
tests =
  checkParallel $$(discover)
