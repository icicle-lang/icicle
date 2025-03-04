{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Icicle.Test.Avalanche.Simp.Linear where

import           Control.Lens.Plated
import           Data.Monoid (Sum(..))


import           Icicle.Avalanche.Statement.Flatten.Algorithms
import           Icicle.Common.Type (ValType(..))

import           P
import           Hedgehog hiding (Var)
import           System.IO
import           Icicle.Common.Base
import           Icicle.Common.Exp
import           Icicle.Common.Fresh
import           Icicle.Internal.Pretty
import           Icicle.Avalanche.Statement.Statement
import qualified Icicle.Avalanche.Prim.Flat as Flat
import qualified Icicle.Avalanche.Prim.Compounds as Flat
import           Icicle.Avalanche.Statement.Simp.Linear (linearise)
import           Icicle.Test.Arbitrary.Core (testFresh)
import           Icicle.Test.Arbitrary.Data (Var)


--
-- This program trivially doesn't need to do the copy,
-- as the very last statement is a copy and nothing after
-- it is used.
simple_copy_elide :: a -> Fresh Var (Statement a Var Flat.Prim)
simple_copy_elide a_fresh = do
  acc  <- freshPrefix "test_arr"
  loc  <- freshPrefix "loc"
  dest <- freshPrefix "dest"

  return
    $ initArr IntT acc (XValue a_fresh (ArrayT IntT) (VArray []))
    $ readArr IntT loc acc
    $ Block [
        Write dest (arrCpy IntT (xVar loc))
      ]

 where
  Flat.FlatOps  {..} = Flat.flatOps a_fresh
  Flat.FlatCons {..} = Flat.flatCons a_fresh

--
-- This program has two copies, one of which can be removed.
-- The first one can not be, because if it were, then the second
-- data point would point to a mutable shared memory location.
simple_copy_retain :: a -> Fresh Var (Statement a Var Flat.Prim)
simple_copy_retain a_fresh = do
  acc    <- freshPrefix "test_arr"
  loc    <- freshPrefix "loc"
  dest   <- freshPrefix "dest"
  dest2  <- freshPrefix "dest"

  return
    $ initArr IntT acc (XValue a_fresh (ArrayT IntT) (VArray []))
    $ readArr IntT loc acc
    $ Block [
        Write dest (arrCpy IntT (xVar loc))
      , Write dest2 (arrCpy IntT (xVar loc))
      ]

 where
  Flat.FlatOps  {..} = Flat.flatOps a_fresh
  Flat.FlatCons {..} = Flat.flatCons a_fresh


--
-- This program uses a write in a block to introduce the
-- shared reference. We need to track that moving forwards through
-- the statements too.
write_ref_copy :: a -> Fresh Var (Statement a Var Flat.Prim)
write_ref_copy a_fresh = do
  acc    <- freshPrefix "test_arr"
  acc2   <- freshPrefix "test_arr"
  loc    <- freshPrefix "loc"
  loc2   <- freshPrefix "loc"
  dest   <- freshPrefix "dest"
  dest2  <- freshPrefix "dest2"

  return
    $ initArr IntT acc (XValue a_fresh (ArrayT IntT) (VArray [VInt 1]))
    $ initArr IntT acc2 (XValue a_fresh (ArrayT IntT) (VArray [VInt 2]))
    $ readArr IntT loc2 acc2
    $ Block[
        Write acc (xVar loc2)
      , readArr IntT loc acc
        $ Block [
            Write dest (arrCpy IntT (xVar loc))
          , Write dest2 (arrCpy IntT (xVar loc2))
          ]
    ]

 where
  Flat.FlatOps  {..} = Flat.flatOps a_fresh
  Flat.FlatCons {..} = Flat.flatCons a_fresh


--
-- This program uses has writes in an if block.
-- Both copies before the last need to be preserved
write_ref_in_if_copy :: a -> Fresh Var (Statement a Var Flat.Prim)
write_ref_in_if_copy a_fresh = do
  acc    <- freshPrefix "test_arr"
  acc2   <- freshPrefix "test_arr"
  acc3   <- freshPrefix "test_arr"
  loc    <- freshPrefix "loc"
  loc2   <- freshPrefix "loc"
  loc3   <- freshPrefix "loc"
  dest   <- freshPrefix "dest"
  dest2  <- freshPrefix "dest2"
  dest3  <- freshPrefix "dest2"

  return
    $ initArr IntT acc (XValue a_fresh (ArrayT IntT) (VArray [VInt 1]))
    $ initArr IntT acc2 (XValue a_fresh (ArrayT IntT) (VArray [VInt 2]))
    $ initArr IntT acc3 (XValue a_fresh (ArrayT IntT) (VArray [VInt 3]))
    $ readArr IntT loc2 acc2
    $ Block[
        If (xTrue) (Write acc (xVar loc2)) (Write acc3 (xVar loc2))
      , readArr IntT loc acc
      $ readArr IntT loc3 acc3
        $ Block [
            Write dest (arrCpy IntT (xVar loc))
          , Write dest2 (arrCpy IntT (xVar loc2))
          , Write dest3 (arrCpy IntT (xVar loc3))
          ]
    ]

 where
  xTrue  = xValue BoolT (VBool True)

  Flat.FlatOps  {..} = Flat.flatOps a_fresh
  Flat.FlatCons {..} = Flat.flatCons a_fresh

--
--
write_ref_out_of_scope_let_copy :: a -> Fresh Var (Statement a Var Flat.Prim)
write_ref_out_of_scope_let_copy a_fresh = do
  acc    <- freshPrefix "test_arr"
  acc2   <- freshPrefix "test_arr"
  loc    <- freshPrefix "loc"
  loc2   <- freshPrefix "loc"
  loc3   <- freshPrefix "loc"
  dest   <- freshPrefix "dest"
  dest2  <- freshPrefix "dest2"

  return
    $ initArr IntT acc (XValue a_fresh (ArrayT IntT) (VArray [VInt 1]))
    $ initArr IntT acc2 (XValue a_fresh (ArrayT IntT) (VArray [VInt 2]))
    $ readArr IntT loc2 acc2
    $ Block[
        Let loc3 (xVar loc2) (Write acc (arrCpy IntT (xVar loc3)))
      , readArr IntT loc acc
        $ Block [
            Write dest (arrCpy IntT (xVar loc))
          , Write dest2 (arrCpy IntT (xVar loc2))
          ]
    ]

 where
  Flat.FlatOps  {..} = Flat.flatOps a_fresh
  Flat.FlatCons {..} = Flat.flatCons a_fresh

--
-- On the first pass, it wouldn't need a copy; but we
-- need one afterwards because acc1 and acc2 point to
-- the same reference.
write_loop :: a -> Fresh Var (Statement a Var Flat.Prim)
write_loop a_fresh = do
  fact   <- freshPrefix "facts"
  acc1   <- freshPrefix "test_arr"
  acc2   <- freshPrefix "test_arr"
  acc3   <- freshPrefix "test_arr"
  loc1   <- freshPrefix "loc"
  loc2   <- freshPrefix "loc"
  loc3   <- freshPrefix "loc"

  return
    $ initArr IntT acc1 (XValue a_fresh (ArrayT IntT) (VArray [VInt 1]))
    $ initArr IntT acc2 (XValue a_fresh (ArrayT IntT) (VArray [VInt 2]))
    $ initArr IntT acc3 (XValue a_fresh (ArrayT IntT) (VArray [VInt 3]))
    $ ForeachFacts (FactBinds fact []) UnitT
    $ readArr IntT loc1 acc1
    $ readArr IntT loc2 acc2
    $ readArr IntT loc3 acc3
    $ Block [
        Write acc3 (arrCpy IntT (xVar loc1))
      , Write acc2 (arrCpy IntT (xVar loc2))
      , Write acc1 (arrCpy IntT (xVar loc2))
    ]

 where
  Flat.FlatOps  {..} = Flat.flatOps a_fresh
  Flat.FlatCons {..} = Flat.flatCons a_fresh


countCopies :: TransformX x => x () n Flat.Prim -> Integer
countCopies a =
  getSum . getConst $
    transformX pure isCopy a


isCopy :: Exp () n Flat.Prim -> Const (Sum Integer) (Exp () n Flat.Prim)
isCopy e =
  case e of
    XPrim () (Flat.PrimArray (Flat.PrimArrayCopy _)) -> Const (Sum 1)
    _ -> plate isCopy e

mkProp :: Integer -> (() -> Fresh Var (Statement () Var Flat.Prim)) -> Property
mkProp expected prog
 = withTests 1 . property $ do
    let built  = testFresh "a" $ prog ()
    let linear = linearise built
    Hedgehog.annotate (show $ pretty built)
    Hedgehog.annotate (show $ pretty linear)
    countCopies linear === expected


prop_elides_copy :: Property
prop_elides_copy
 = mkProp 0 simple_copy_elide

prop_keeps_copy :: Property
prop_keeps_copy
 = mkProp 1 simple_copy_retain

prop_write_ref_copy :: Property
prop_write_ref_copy
 = mkProp 1 write_ref_copy

prop_write_ref_in_if_copy :: Property
prop_write_ref_in_if_copy
 = mkProp 2 write_ref_in_if_copy

prop_write_ref_out_of_scope_let_copy :: Property
prop_write_ref_out_of_scope_let_copy
 = mkProp 2 write_ref_out_of_scope_let_copy

prop_fact_loop :: Property
prop_fact_loop
 = mkProp 2 write_loop

tests :: IO Bool
tests =
  checkParallel $$(discover)
