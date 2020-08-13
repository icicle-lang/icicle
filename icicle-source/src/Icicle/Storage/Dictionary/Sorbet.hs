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

import           Icicle.Compiler.Source
import           Icicle.Dictionary.Data
import qualified Icicle.Source.Query                           as Q

import           Icicle.Storage.Dictionary.Common

import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class

import           System.FilePath
import           System.IO

import qualified Data.ByteString                               as ByteString
import qualified Data.Map                                      as Map
import qualified Data.Set                                      as Set
import qualified Data.Text.Encoding                            as Text

import           P

-- Top level IO function which loads all dictionaries and imports
loadDictionary :: CheckOptions -> FilePath -> FilePath -> EitherT DictionaryImportError IO Dictionary
loadDictionary checkOpts rootdir fname = do
  input   <- Text.decodeUtf8 <$> liftIO (ByteString.readFile fname)
  natural <- firstEitherT DictionaryErrorCompilation $ readIcicleLibrary checkOpts rootdir fname input

  let
    tombstones =
      Set.singleton "NA"

    inputs =
      fmap (\(Q.ModuleInput _ iid enc key) ->
        DictionaryInput iid enc tombstones key)

    outputs =
      Map.mapWithKey DictionaryOutput

  pure $
    Dictionary
      (inputs      $ Q.resolvedInputs natural)
      (outputs     $ Q.resolvedOutputs natural)
      (builtinFunctions <> Q.resolvedEntries natural)
