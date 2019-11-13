{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Arbitrary.Core where

import           Icicle.Data hiding (Value(..), StructField(..))

import           Icicle.Common.Base
import           Icicle.Common.Eval
import           Icicle.Common.Exp
import           Icicle.Common.Type
import           Icicle.Common.Value
import qualified Icicle.Common.Fresh                as Fresh
import           Icicle.Common.NanEq

import qualified Icicle.Core.Exp                as X
import           Icicle.Core.Exp.Prim
import           Icicle.Core.Program.Program    as P

import           Icicle.Test.Arbitrary.Data

import qualified Test.QuickCheck.Hedgehog as Qc

import qualified Icicle.Test.Gen.Core.Value as CoreGen
import qualified Icicle.Test.Gen.Core.Program as CoreGen
import qualified Icicle.Test.Gen.Core.Type as CoreGen

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P


testFreshT :: Functor m => Text -> Fresh.FreshT Var m a -> m a
testFreshT desc prog
 = fmap snd
 $ Fresh.runFreshT prog
 $ Fresh.counterNameState (NameBase . Var desc) 0

testFresh :: Text -> Fresh.Fresh Var a -> a
testFresh desc prog
 = snd
 $ Fresh.runFresh prog
 $ Fresh.counterNameState (NameBase . Var desc) 0

-- | Check if values are equal except for functions/closures
-- Because closure heaps can differ..
equalExceptFunctions :: (NanEq a, NanEq n, NanEq p) => Value a n p -> Value a n p -> Bool
equalExceptFunctions p q
 | VFun{} <- p
 , VFun{} <- q
 = True
 | otherwise
 = p `nanEq` q

equalExceptFunctionsE :: (NanEq l, NanEq a, NanEq n, NanEq p)
                      => Either l (Value a n p)
                      -> Either l (Value a n p)
                      -> Bool
equalExceptFunctionsE p q
 | Right p' <- p
 , Right q' <- q
 = p' `equalExceptFunctions` q'
 | otherwise
 = p `nanEq` q


-- | Generate a well typed expression.
-- If we can't generate a well typed expression we want quickcheck to count it as
-- failing to satisfy a precondition.
withTypedExp :: Testable prop => (Exp () Var Prim -> ValType -> prop) -> Property
withTypedExp prop
 = forAll genCoreExp
 $ \(x, t)
 -> typeExp0 X.coreFragment x == Right (FunT [] t) ==> prop x t

genCoreExp :: Gen (Exp () Var Prim, ValType)
genCoreExp = do
 Qc.hedgehog $ CoreGen.genExpTop

genCoreExpNoType :: Gen (Exp () Var Prim)
genCoreExpNoType = fst <$> genCoreExp

programForStreamType :: ValType -> Gen (Program () Var)
programForStreamType i = do
 o <- genOutputType
 Qc.hedgehog $ CoreGen.programForStreamType i o

programForStreamTypeWithOutput :: ValType -> ValType -> Gen (Program () Var)
programForStreamTypeWithOutput i o = Qc.hedgehog $ CoreGen.programForStreamType i o

genInputType :: Gen ValType
genInputType = Qc.hedgehog $ CoreGen.genInputType

genOutputType :: Gen ValType
genOutputType = Qc.hedgehog $ CoreGen.genOutputType

baseValueForType :: ValType -> Gen BaseValue
baseValueForType = Qc.hedgehog . CoreGen.baseValueForType

inputsForType :: ValType -> Gen ([AsAt BaseValue], EvalContext)
inputsForType = Qc.hedgehog . CoreGen.inputsForType

instance Arbitrary ValType where
 arbitrary = Qc.hedgehog CoreGen.genValType


listOfN :: Int -> Int -> Gen a -> Gen [a]
listOfN m n gen =
  sized $ \size -> do
    let
      diff =
        size * (n - m) `quot` 99

    k <- choose (m, m + diff)
    vectorOf k gen
