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


  , lexCheckDictionary
  ) where

import           Icicle.Dictionary.Data

import           Icicle.Internal.Pretty                        hiding ((</>))

import           Icicle.Sorbet.Parse                           (ParseError (..))
import           Icicle.Sorbet.Lexical.Lexer
import           Icicle.Sorbet.Lexical.Layout
import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position


import           Icicle.Source.Checker                         (CheckOptions (..))

import           Icicle.Storage.Dictionary.Common
import           Icicle.Storage.Dictionary.Data
import           Icicle.Storage.Dictionary.Sorbet.Parser


import qualified Control.Exception                             as E
import           Control.Monad.Trans.Either

import           System.FilePath
import           System.IO

import qualified Data.Text.IO                                  as T

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
    lexed  <- first LexParseError      $ Mega.runParser (consumeAll lexProgram) "" inputText
    layed  <- first LayoutParseError   $ layoutProgram lexed
    parsed <- first AbstractParseError $ Mega.runParser (consumeAll pDictionary) "" (PositionedStream inputText layed)
    return parsed



lexCheckDictionary :: FilePath -> EitherT DictionaryImportError IO Bool
lexCheckDictionary dictPath = do
  inputText <- firstEitherT DictionaryErrorIO . EitherT $ E.try (T.readFile dictPath)
  firstEitherT DictionaryErrorSorbet . hoistEither $ do
    lexed  <- first LexParseError      $ Mega.runParser (consumeAll lexProgram) "" inputText
    case lexed of
      Positioned _ _ Tok_Dictionary : _
        -> pure True
      _ -> pure False



consumeAll :: MonadParsec e s m => m a ->  m a
consumeAll f = do
 r <- f
 Mega.eof
 return r

