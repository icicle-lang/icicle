{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.Convert where

import           Data.Monoid (Any (..))

import           Icicle.Test.Arbitrary
import           Icicle.Internal.Pretty
import qualified Icicle.Core.Program.Check      as Check
import           Icicle.Source.Transform.Base
import qualified Icicle.Source.Query.Exp        as Source
import qualified Icicle.Source.Query            as Query

import           P

import           System.IO

import           Hedgehog
import qualified Hedgehog.Gen.QuickCheck as QuickCheck
import           Control.Monad.Trans.Writer (Writer, tell, execWriter)


prop_source_convert :: Property
prop_source_convert
 = withTests 1000 . withDiscards 50000 . property $ do
    qwf <- forAllWith qwfPretty QuickCheck.arbitrary
    case (qwfCheck qwf, qwfCheckKey qwf) of
      (Right qt', Right k') -> do
        classify "case"   $ hasExpressions expIsCase   qt'
        classify "if let" $ hasExpressions expIsIfLet  qt'
        classify "if"     $ hasExpressions expIsIf     qt'
        classify "var"    $ hasExpressions expIsVar    qt'
        classify "access" $ hasExpressions expIsAccess qt'
        classify "record" $ hasExpressions expIsAccess qt'

        core <- evalEither $ first pretty $ qwfConvertToCore qwf k' qt'
        void  $ evalEither $ first pretty $ Check.checkProgram core
      _
        -> discard


type Seen = Writer Any

progress :: a -> Seen a
progress a = do
  tell (Any True)
  return a

hasExpressions :: (Query.Exp a n -> Bool) -> Query.QueryTop a n -> Bool
hasExpressions func =
  getAny . execWriter . transformQT (hasExprTransform func)


hasExprTransform :: (Query.Exp a n -> Bool) -> Transform Seen () a n
hasExprTransform condition =
  Transform {
    transformExp = \_ e -> if condition e then progress ((), e) else return ((), e)
  , transformPat = nope
  , transformContext = nope
  , transformState = ()
  }


nope :: Monad m => p -> b -> m ((), b)
nope _ e = return ((), e)

expIsCase :: Source.Exp' q a n -> Bool
expIsCase = \case
  Source.Case {} -> True
  _ -> False

expIsIfLet :: Source.Exp' q a n -> Bool
expIsIfLet = \case
  Source.IfLet {} -> True
  _ -> False

expIsIf :: Source.Exp' q a n -> Bool
expIsIf = \case
  Source.If {} -> True
  _ -> False

expIsVar :: Source.Exp' q a n -> Bool
expIsVar = \case
  Source.Var {} -> True
  _ -> False

expIsAccess :: Source.Exp' q a n -> Bool
expIsAccess = \case
  Source.Access {} -> True
  _ -> False

expIsRecord :: Source.Exp' q a n -> Bool
expIsRecord = \case
  Source.Record {} -> True
  _ -> False

tests :: IO Bool
tests =
  checkParallel $$(discover)
