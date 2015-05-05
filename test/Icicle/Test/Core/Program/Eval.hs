{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Program.Eval where

import           Icicle.Test.Core.Arbitrary
-- import           Icicle.Core.Program.Program
import           Icicle.Core.Program.Check
-- import qualified Icicle.Core.Eval.Exp       as XV
import qualified Icicle.Core.Eval.Program   as PV

import           Icicle.Data.DateTime

import           P

import           System.IO

import           Test.QuickCheck

-- Just choose some date; it doesn't matter
someDate = dateOfYMD 2015 1 1

-- Well typed programs don't go wrong
-- =====================
--
-- Restrict it to only good programs.
prop_progress t =
 forAll (programForStreamType t)
 $ \p ->
    isRight     (checkProgram p) ==> isRight (PV.eval someDate [] p)

-- Also, try the inverse: if it has a runtime error, it can't be type safe.
-- Most randomly generated programs will have runtime errors, and won't type check
prop_progress_inverse x =
 isLeft      (PV.eval someDate [] x)
 ==> isLeft  (checkProgram x)

-- It would be nice to say something about inputs,
-- but we need to be able to generate well typed values.
-- A well typed program can still error if its inputs are of the wrong type.



return []
tests :: IO Bool
tests = $quickCheckAll
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10, maxDiscardRatio = 10000})