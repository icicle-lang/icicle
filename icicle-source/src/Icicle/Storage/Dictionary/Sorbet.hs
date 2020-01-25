{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Storage.Dictionary.Sorbet (
    DictionaryImportError (..)
  , ImplicitPrelude (..)
  , loadDictionary
  , prelude
  ) where

import           Icicle.Dictionary.Data

import           Icicle.Internal.Pretty                        hiding ((</>))

import qualified Icicle.Sorbet.Abstract.Parser                 as Sorbet
import qualified Icicle.Sorbet.Abstract.Tokens                 as Sorbet
import qualified Icicle.Sorbet.Abstract.Type                   as Sorbet
import           Icicle.Sorbet.Parse                           (ParseError (..))
import           Icicle.Sorbet.Lexical.Lexer
import           Icicle.Sorbet.Lexical.Layout
import           Icicle.Sorbet.Position


import           Icicle.Source.Checker                         (CheckOptions (..))
import           Icicle.Source.Query                           (QueryTop (..), Query (..), Exp, Decl)
import qualified Icicle.Source.Query                           as SQ

import           Icicle.Storage.Dictionary.Common
import           Icicle.Storage.Dictionary.Data
import           Icicle.Storage.Dictionary.Sorbet.Parser


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

import qualified Text.Megaparsec as Mega
import           Text.Megaparsec (MonadParsec)

import           P

-- Top level IO function which loads all dictionaries and imports
loadDictionary :: CheckOptions -> ImplicitPrelude -> FilePath -> EitherT DictionaryImportError IO Dictionary
loadDictionary checkOpts impPrelude dictionary
 = loadDictionary' checkOpts impPrelude (dictionaryFunctions emptyDictionary) mempty parseSorbet [] dictionary


parseSorbet :: DictionaryConfig
            -> FilePath
            -> EitherT DictionaryImportError IO (DictionaryConfig, [DictionaryInput'], [DictionaryOutput'])
parseSorbet _ dictPath = do
  inputText <- firstEitherT DictionaryErrorIO . EitherT $ E.try (T.readFile dictPath)
  firstEitherT DictionaryErrorSorbet . hoistEither $ do
    lexed  <- first LexParseError $ Mega.runParser (consumeAll lexProgram) "" inputText
    layed  <- first LayoutParseError $ layoutProgram lexed
    parsed <- first AbstractParseError $ Mega.runParser (consumeAll pDictionary) "" (PositionedStream inputText layed)
    return parsed


consumeAll :: MonadParsec e s m => m a ->  m a
consumeAll f = do
 r <- f
 Mega.eof
 return r
