{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl.Load (
    loadFile
  , showDictionary
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (hoist)
import           Control.Monad.State (modify, gets)
import           Control.Monad.Trans.Either (runEitherT)
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.List as List

import qualified Icicle.Compiler.Source as Source
import           Icicle.Dictionary      as Dictionary
import qualified Icicle.Internal.Pretty as Pretty
import           Icicle.Repl.Data
import           Icicle.Repl.Monad
import           Icicle.Repl.Pretty
import           Icicle.Repl.Source
import qualified Icicle.Runtime.Serial.Zebra as Runtime
import qualified Icicle.Storage.Dictionary.Sorbet as Sorbet
import qualified Icicle.Source.Query as Query

import           P

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.IO (FilePath)
import qualified System.IO as IO

import qualified Viking.ByteStream as ByteStream

import qualified Zebra.Serial.Binary as Binary
import qualified Zebra.Table.Schema as Schema
import           Zebra.X.Either (firstJoin)


data LoadType =
    LoadSorbet
  | LoadData
    deriving (Eq, Ord, Show)

isIcicle :: FilePath -> Bool
isIcicle =
  (== ".icicle") . FilePath.takeExtension

detectLoadType :: MonadIO m => FilePath -> m LoadType
detectLoadType path =
  if isIcicle path then
    pure LoadSorbet
  else
    pure LoadData

showDictionary :: Repl ()
showDictionary = do
  dictionary <- gets stateDictionary
  putSection "Dictionary" $
    prettyDictionarySummary dictionary

setDictionary :: Dictionary -> Repl ()
setDictionary dictionary = do
  liftIO . IO.putStrLn $
     "Loaded dictionary with " <>
     show (length $ dictionaryInputs dictionary) <> " inputs, " <>
     show (length $ dictionaryOutputs dictionary) <> " outputs, " <>
     show (length $ dictionaryFunctions dictionary) <> " functions."

  modify $ \s ->
    s { stateDictionary = dictionary }


loadSorbet :: FilePath -> Repl ()
loadSorbet path = do
  loadSorbetDictionary path


loadSorbetDictionary :: FilePath -> Repl ()
loadSorbetDictionary path = do
  let
    rootDir
      = FilePath.takeDirectory path

  options     <- getCheckOptions
  edictionary <- liftIO . runEitherT $ Sorbet.loadDictionary options rootDir path
  case edictionary of
    Left err -> do
      putPretty $ Pretty.vsep [
          "Dictionary load error:"
        , Pretty.indent 2 $ Pretty.pretty err
        ]

    Right dictionary ->
      setDictionary dictionary


loadFunctionsFrom :: FilePath -> Text -> Repl ()
loadFunctionsFrom path src = do
  let
    rootDir
      = FilePath.takeDirectory path

  opts <- getCheckOptions
  ret  <- liftIO . runEitherT $
    Source.readIcicleLibrary opts rootDir path src

  case ret of
   Left err ->
     putPretty err

   Right module0 -> do
     let
       functions0 =
         Query.resolvedEntries module0

     liftIO . IO.putStrLn $
       "Loaded " <> show (length functions0) <> " functions from " <> path

     dictionary <- gets stateDictionary

     -- Merge in the source file with new functions taking precedence over existing ones
     let
       functions =
         List.nubBy ((==) `on` functionName) $
           functions0 <>
           dictionaryFunctions dictionary

     modify $ \s ->
       s { stateDictionary = dictionary { dictionaryFunctions = functions } }

tryLoadZebra :: MonadIO m => FilePath -> m (Maybe Schema.Table)
tryLoadZebra path =
  liftIO . fmap rightToMaybe . runResourceT . runEitherT $ do
    -- FIXME zebra should have a faster way to get the schema
    (schema, _) <-
      firstJoin show .
        Binary.decodeLogical .
      hoist (firstT show) $
        ByteStream.readFile path
    pure schema

loadFile :: FilePath -> Repl ()
loadFile path = do
  ok <- liftIO $ Directory.doesFileExist path

  if not ok then
    liftIO . IO.putStrLn $ "Failed, file does not exist: " <> path

  else do
    typ <- detectLoadType path
    case typ of
      LoadSorbet ->
        loadSorbet path

      LoadData -> do
        mschema <- tryLoadZebra path
        case mschema of
          Nothing -> do
            liftIO . IO.putStrLn $ "Selected psv file as input: " <> path
            modify $ \s ->
              s { stateInput = InputPsv path }

          Just schema ->
            case Runtime.decodeDictionary schema of
              Left err ->
                putPretty (show err) -- FIXME pretty

              Right dictionary -> do
                setDictionary dictionary
                uncurry loadFunctionsFrom Dictionary.prelude
                liftIO . IO.putStrLn $ "Selected zebra binary file as input: " <> path
                modify $ \s ->
                  s { stateInput = InputZebra path }
