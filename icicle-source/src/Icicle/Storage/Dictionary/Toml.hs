{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Storage.Dictionary.Toml (
    DictionaryImportError (..)
  , ImplicitPrelude (..)
  , loadDictionary
  , prelude
  ) where

import           Icicle.Dictionary.Data

import           Icicle.Internal.Pretty                        hiding ((</>))

import           Icicle.Source.Checker                         (CheckOptions (..))
import qualified Icicle.Source.Parser                          as SP
import           Icicle.Source.Query                           (QueryTop (..), Query (..), Exp, Decl)
import qualified Icicle.Source.Query                           as SQ

import           Icicle.Storage.Dictionary.Common
import           Icicle.Storage.Dictionary.Data
import           Icicle.Storage.Dictionary.Toml.Toml
import           Icicle.Storage.Dictionary.Toml.TomlDictionary
import           Icicle.Storage.Dictionary.Toml.Types

import qualified Control.Exception                             as E
import           Control.Monad.Trans.Either

import qualified Data.Map.Strict                               as Map

import           System.FilePath
import           System.IO

import qualified Data.Set                                      as Set
import qualified Data.Text                                     as T
import qualified Data.Text.IO                                  as T
import           Data.Validation (toEither)

import           Text.Parsec                                   (SourcePos)
import qualified Text.Parsec                                   as Parsec
import qualified Text.Parsec.Pos                               as Parsec

import           P

-- Top level IO function which loads all dictionaries and imports
loadDictionary :: CheckOptions -> ImplicitPrelude -> FilePath -> EitherT DictionaryImportError IO Dictionary
loadDictionary checkOpts impPrelude dictionary
 = loadDictionary' checkOpts impPrelude (dictionaryFunctions emptyDictionary) mempty parseLoadToml [] dictionary


parseLoadToml :: DictionaryConfig
              -> FilePath
              -> EitherT DictionaryImportError IO (DictionaryConfig, [DictionaryInput'], [DictionaryOutput'])
parseLoadToml  parentConf dictPath = do
  rawToml              <- parseTOML dictPath
  firstEitherT DictionaryErrorParse . hoistEither . toEither $ tomlDict parentConf rawToml


parseTOML :: FilePath -> EitherT DictionaryImportError IO Table
parseTOML dictPath = do
  inputText <- firstEitherT DictionaryErrorIO . EitherT $ E.try (readFile dictPath)
  rawToml   <- firstEitherT DictionaryErrorParsecTOML . hoistEither $ Parsec.parse tomlDoc dictPath inputText
  return rawToml
