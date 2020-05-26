{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Storage.Dictionary.Data (
    ImplicitPrelude (..)
  , DictionaryConfig (..)
  , DictionaryInput' (..)
  , DictionaryOutput' (..)
  , ConcreteKey' (..)
  ) where

import           Icicle.Common.Type
import           Icicle.Data
import qualified Icicle.Sorbet.Position as Sorbet
import           Icicle.Source.Query
import           Icicle.Source.Lexer.Token (Variable)

import           P


-- | Dictionary config can be inherited from higher level dictionaries, items such as
--   Namespace and tombstone can be scoped based on where they are defined, and overridden
--   inside a specific fact or feature.
data DictionaryConfig =
  DictionaryConfig {
    configTitle     :: Maybe Text
  , configVersion   :: Maybe Int64
  , configNamespace :: Maybe Namespace
  , configTombstone :: Maybe Text
  , configImports   :: [Text]
  , configChapter   :: [Text]
  } deriving (Eq, Show)

instance Semigroup DictionaryConfig where
  -- Left preferenced Semigroup instance.
  -- Use properties specified in this file, or, if they don't exist, try the parent.
  -- Don't bring in the imports or chapters, as that will cause an infinite loop.
  (<>)
    (DictionaryConfig a1 a2 a3 a4 a6 a7)
    (DictionaryConfig b1 b2 b3 b4 _  _ ) =
      (DictionaryConfig (a1 <|> b1) (a2 <|> b2) (a3 <|> b3) (a4 <|> b4) a6 a7)

instance Monoid DictionaryConfig where
  mempty  = DictionaryConfig Nothing Nothing Nothing Nothing [] []
  mappend = (<>)


data DictionaryInput' =
  DictionaryInput' {
      inputId' :: InputId
    , inputEncoding' :: ValType
    , inputTombstone' :: Maybe Text
    , inputKey' :: ConcreteKey'
    } deriving (Eq, Show)


data DictionaryOutput' =
  DictionaryOutput' {
      outputId' :: OutputId
    , outputQuery' :: QueryTop Sorbet.Range Variable
    } deriving (Eq, Show)


newtype ConcreteKey' = ConcreteKey' {
    concreteKey :: Maybe (Exp Sorbet.Range Variable)
  } deriving (Eq, Show)


data ImplicitPrelude = ImplicitPrelude | NoImplicitPrelude
  deriving (Eq, Ord, Show)
