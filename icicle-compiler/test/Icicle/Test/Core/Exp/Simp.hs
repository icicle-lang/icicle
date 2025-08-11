{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Exp.Simp where

import Icicle.Internal.Pretty (pretty)
import Icicle.Test.Gen.Core.Program
import Icicle.Test.Arbitrary.Data
import Icicle.Test.Arbitrary.NanEq (hedgehogNanEq)
import Hedgehog hiding (Var)

import           Icicle.Core.Eval.Exp
import qualified Icicle.Core.Exp.Simp               as CoreSimp
import           Icicle.Core.Exp (coreFragment)
import           Icicle.Common.Exp
import qualified Icicle.Common.Exp.Simp.Beta        as Beta
import qualified Icicle.Common.Exp.Simp.ANormal     as ANormal
import qualified Icicle.Common.Fresh                as Fresh
import           Icicle.Common.Base

import           P

import           System.IO

-- TODO XXX FIXME: argh! beta isn't being tested properly because we aren't generating applied lambdas!

-- Performing beta reduction
prop_beta_evaluation :: Property
prop_beta_evaluation = property $ do
  x <- forAll (fst <$> genExpTop)
  let x' = Beta.beta x
  annotate (show $ pretty x)
  annotate (show $ pretty x')
  eval0 evalPrim x `hedgehogNanEq` eval0 evalPrim x'

-- Beta reduction preserves type
prop_beta_type :: Property
prop_beta_type = property $ do
  x <- forAll (fst <$> genExpTop)
  typeExp0 coreFragment x === typeExp0 coreFragment ( Beta.beta x)

-- Converting all beta reductions to lets
prop_betaToLets_evaluation :: Property
prop_betaToLets_evaluation = property $ do
  x <- forAll (fst <$> genExpTop)
  let x' = Beta.betaToLets () x
  annotate (show $ pretty x)
  annotate (show $ pretty x')
  eval0 evalPrim x `hedgehogNanEq` eval0 evalPrim x'

-- Beta reduction preserves type
prop_betaToLets_type :: Property
prop_betaToLets_type = property $ do
  x <- forAll (fst <$> genExpTop)
  typeExp0 coreFragment x === typeExp0 coreFragment ( Beta.betaToLets () x)



runFresh :: Fresh.Fresh Var b -> b
runFresh func =
  snd $
    Fresh.runFresh func
      (Fresh.counterNameState (NameBase . Var "test") 0)


-- Converting to a-normal form
prop_anormal_form_evaluation :: Property
prop_anormal_form_evaluation = property $ do
  x <- forAll (fst <$> genExpTop)
  eval0 evalPrim x `hedgehogNanEq` eval0 evalPrim
    (runFresh (ANormal.anormal () x))


-- Converting to a-normal form preserves type
prop_anormal_form_type :: Property
prop_anormal_form_type = property $ do
  x <- forAll (fst <$> genExpTop)
  let x' = runFresh (ANormal.anormal () x)
  annotate (show $ pretty x)
  annotate (show $ pretty x')
  typeExp0 coreFragment x === typeExp0 coreFragment x'


-- Core simplification preserves type
prop_core_simp_type :: Property
prop_core_simp_type = property $ do
  x <- forAll (fst <$> genExpTop)
  let simple = runFresh (CoreSimp.simp () x)
  annotate (show . pretty $ x)
  annotate (show . pretty $ simple)
  typeExp0 coreFragment x === typeExp0 coreFragment simple


-- Core simplification preserves result
prop_core_simp_eval :: Property
prop_core_simp_eval = withTests 500 $ property $ do
  x <- forAll (fst <$> genExpTop)
  annotate (show . pretty $ x)
  let x' = runFresh (CoreSimp.simp () x)
  annotate (show . pretty $ x')
  eval0 evalPrim x `hedgehogNanEq` eval0 evalPrim x'


tests :: IO Bool
tests = checkParallel $$(discover)
