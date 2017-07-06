{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.MaxMapSize where

import           Icicle.Test.Arbitrary.SourceWithCore
import           Icicle.Test.Arbitrary.Corpus
import           Icicle.Test.Arbitrary
import qualified Icicle.Core.Eval.Program   as PV

import           Icicle.Common.Eval
import           Icicle.Common.Base
import           Icicle.Data.Fact (AsAt(..))

import           P

import           System.IO

import           Test.QuickCheck

import           Control.Arrow ((***))
import qualified Data.List as List
import qualified Data.Map as Map


prop_maxsize :: TestSourceConvert -> Property
prop_maxsize ts =
  let maxsize = evalMaxMapSize $ tsEvalCtx ts
      inputs' = fmap (\(AsAt (bg,v) t) -> AsAt (bg, clip maxsize v) t)
              $ tsInputs ts
      ev      = evalCore ts inputs'
  in counterexample (show ev)
   $ case ev of
      Left _ -> property False
      Right r  ->
       let vals  = fmap snd $ PV.value r
           props = fmap (checkMaxMapSize maxsize) vals
       in  conjoin props

prop_maxsize_corpus :: Property
prop_maxsize_corpus = testAllCorpus $ \wt ->
  let maxsize = wtMaxMapSize wt
      facts'  = fmap (\(AsAt v t) -> AsAt (clip maxsize v) t)
              $ wtFacts wt
      ev      = evalWellTyped wt { wtFacts = facts' }
      vals    = fmap snd ev
      props   = fmap (checkMaxMapSize maxsize) vals
  in counterexample (show ev)
   $ conjoin props

-- Clip any map inputs to strictly below the maximum size before running evaluator.
clip :: Int -> BaseValue -> BaseValue
clip maxMapSize = go
 where
  go v = case v of
   VMap m
    -> VMap
     $ Map.fromList
     $ fmap (go *** go)
     $ List.take (maxMapSize - 1)
     $ Map.toList m

   VBuf vs
    -> VBuf $ fmap go vs
   VArray vs
    -> VArray $ fmap go vs

   VInt    _ -> v
   VDouble _ -> v
   VUnit     -> v
   VBool   _ -> v
   VTime   _ -> v
   VString _ -> v
   VPair a b -> VPair (go a) (go b)
   VLeft   a -> VLeft   $ go a
   VRight  a -> VRight  $ go a
   VNone     -> v
   VSome   a -> VSome   $ go a
   VStruct m -> VStruct $ fmap go m
   VError  _ -> v
   VFactIdentifier _ -> v


checkMaxMapSize :: Int -> BaseValue -> Property
checkMaxMapSize maxMapSize = go
 where
  go v = case v of
   VMap m
    | checkLength m
    -> conjoin (fmap go $ Map.keys m) .&&. conjoin (fmap go $ Map.elems m)
    | otherwise
    -> counterexample (show m) False

   -- Arbitrary-sized buffers are allowed for now.
   -- We need to allow arrays too, since buffers get converted to arrays. Darn.
   VBuf vs
    -> conjoin (fmap go vs)
   VArray vs
    -> conjoin (fmap go vs)

   VInt    _ -> ok
   VDouble _ -> ok
   VUnit     -> ok
   VBool   _ -> ok
   VTime   _ -> ok
   VString _ -> ok
   VPair a b -> go a .&&. go b
   VLeft   a -> go a
   VRight  a -> go a
   VNone     -> ok
   VSome   a -> go a
   VStruct m -> conjoin (fmap go $ Map.elems m)
   VError  _ -> ok
   VFactIdentifier _ -> ok

  ok = property True

  checkLength vs
   = length vs < maxMapSize || maxMapSize <= 0


return []
tests :: IO Bool
tests = $checkAllWith TestRunMore (checkArgsSized 100)