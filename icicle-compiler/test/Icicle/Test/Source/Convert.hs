{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}
module Icicle.Test.Source.Convert where

import           Icicle.Test.Arbitrary
import           Icicle.Internal.Pretty
import qualified Icicle.Core.Program.Check      as CCheck
import           Icicle.Source.Query (Exp, Query(..), Context, Context' (..), Exp' (..))
import           Icicle.Source.Query.Exp (Prim(..))

import           Icicle.Source.Transform.Base

import           P

import           System.IO

import           Hedgehog
import qualified Hedgehog.Gen.QuickCheck as QuickCheck
import           Data.Monoid (Any(..))
import           Control.Monad.Trans.Writer


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
              checkCoverages qwf
              void $ evalEither $ first pretty $ CCheck.checkProgram conv
      _
        -> discard

checkCoverages :: MonadTest m => QueryWithFeature -> m ()
checkCoverages qwf = do
  let
    queryHasContext f =
      hasContext f (qwfQuery qwf)
    queryHasExpr f =
      hasExpr f (qwfQuery qwf)

  classify "Let"        $ queryHasContext letCtx
  classify "Let Scan"   $ queryHasContext letScanCtx
  classify "Let Fold"   $ queryHasContext letFoldCtx
  classify "Group"      $ queryHasContext groupCtx
  classify "Group Fold" $ queryHasContext groupFoldCtx
  classify "Window"     $ queryHasContext windowCtx
  classify "Latest"     $ queryHasContext latestCtx
  classify "Filter"     $ queryHasContext filterCtx
  classify "Filter Let" $ queryHasContext filterLetCtx
  classify "Distinct"   $ queryHasContext distinctCtx

  classify "App"        $ queryHasExpr appExpr
  classify "Prim"       $ queryHasExpr primExpr
  classify "If"         $ queryHasExpr ifExpr
  classify "Case"       $ queryHasExpr caseExpr
  classify "Access"     $ queryHasExpr accessExpr
  classify "Record"     $ queryHasExpr recordExpr
  classify "Var"        $ queryHasExpr varExpr
  classify "Lit"        $ queryHasExpr litExpr
  classify "Nested"     $ queryHasExpr nestedExpr


letCtx :: Context a n -> Bool
letCtx = \case
  Let {} -> True
  _ -> False

letScanCtx :: Context a n -> Bool
letScanCtx = \case
  LetScan {} -> True
  _ -> False

letFoldCtx :: Context a n -> Bool
letFoldCtx = \case
  LetFold {} -> True
  _ -> False

groupCtx :: Context a n -> Bool
groupCtx = \case
  GroupBy {} -> True
  _ -> False

groupFoldCtx :: Context a n -> Bool
groupFoldCtx = \case
  GroupFold {} -> True
  _ -> False

windowCtx :: Context a n -> Bool
windowCtx = \case
  Windowed {} -> True
  _ -> False

latestCtx :: Context a n -> Bool
latestCtx = \case
  Latest {} -> True
  _ -> False

filterCtx :: Context a n -> Bool
filterCtx = \case
  Filter {} -> True
  _ -> False

filterLetCtx :: Context a n -> Bool
filterLetCtx = \case
  FilterLet {} -> True
  _ -> False

distinctCtx :: Context a n -> Bool
distinctCtx = \case
  Distinct {} -> True
  _ -> False

appExpr :: Exp a n -> Bool
appExpr = \case
  App {} -> True
  _ -> False

ifExpr :: Exp a n -> Bool
ifExpr = \case
  If {} -> True
  _ -> False

caseExpr :: Exp a n -> Bool
caseExpr = \case
  Case {} -> True
  _ -> False

accessExpr :: Exp a n -> Bool
accessExpr = \case
  Access {} -> True
  _ -> False

recordExpr :: Exp a n -> Bool
recordExpr = \case
  Record {} -> True
  _ -> False

varExpr :: Exp a n -> Bool
varExpr = \case
  Icicle.Source.Query.Var {} -> True
  _ -> False

primExpr :: Exp a n -> Bool
primExpr = \case
  Prim _ (Lit _) -> False
  Prim {} -> True
  _ -> False

litExpr :: Exp a n -> Bool
litExpr = \case
  Prim _ (Lit _) -> True
  _ -> False

nestedExpr :: Exp a n -> Bool
nestedExpr = \case
  Nested {} -> True
  _ -> False


hasContext :: (Context a n -> Bool) -> Query a n -> Bool
hasContext f query =
  let
    notingTransform =
      idTransform {
        transformContext = \() ctx -> do
          when (f ctx) $ tell (Any True)
          return ((), ctx)
      }

  in
  getAny $ execWriter $
    transformQ notingTransform query

hasExpr :: (Exp a n -> Bool) -> Query a n -> Bool
hasExpr f query =
  let
    notingTransform =
      idTransform {
        transformExp = \() ctx -> do
          when (f ctx) $ tell (Any True)
          return ((), ctx)
      }

  in
  getAny $ execWriter $
    transformQ notingTransform query


tests :: IO Bool
tests =
  checkParallel $$(discover)
