{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.Convert where

import           Icicle.Test.Arbitrary
import           Icicle.Internal.Pretty
import qualified Icicle.Core.Program.Check      as CCheck

import           P

import           System.IO

import           Hedgehog
import qualified Hedgehog.Gen.QuickCheck as QuickCheck


prop_convert_ok :: Property
prop_convert_ok
 = withTests 1000 . withDiscards 50000 . property $ do
    qwf <- forAllWith qwfPretty QuickCheck.arbitrary
    case (qwfCheck qwf, qwfCheckKey qwf) of
      (Right qt', Right k')
        -> void $ evalEither $ first pretty $ qwfConvertToCore qwf k' qt'
      _
        -> discard


prop_convert_is_well_typed :: Property
prop_convert_is_well_typed
 = withTests 1000 . withDiscards 50000 . property $ do
    qwf <- forAllWith qwfPretty QuickCheck.arbitrary
    case (qwfCheck qwf, qwfCheckKey qwf) of
      (Right qt', Right k')
        | Right conv <- qwfConvertToCore qwf k' qt'
        -> do Hedgehog.annotateShow $ pretty conv
              void $ evalEither $ first pretty $ CCheck.checkProgram conv
      _
        -> discard


tests :: IO Bool
tests =
  checkParallel $$(discover)
