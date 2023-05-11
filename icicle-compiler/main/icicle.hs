{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import           Paths_icicle (version)
import           Control.Monad.Trans.Either.Exit (orDie)

import           Data.Version (showVersion)

import           Data.String (String, fromString)
import qualified Data.Text as Text

import           GHC.Conc (getNumProcessors, setNumCapabilities)

import           Icicle.Command.Compile
import           Icicle.Command.Query
import           Icicle.Data.Time (dateOfText)
import           Icicle.Repl
import           Icicle.LSP.Driver

import           P

import           System.IO (IO, FilePath, BufferMode(..))
import           System.IO (hSetBuffering, stdout, stderr)

import           X.Options.Applicative (Parser, Mod, CommandFields)
import qualified X.Options.Applicative as Options


data IcicleCommand =
    IcicleRepl !ReplOptions
  | IcicleCompile !Compile
  | IcicleQuery !Query
  | IcicleCheck !Check
  | IcicleLSP !(Maybe FilePath)
    deriving (Eq, Ord, Show)

main :: IO ()
main = do
  n <- getNumProcessors
  setNumCapabilities (min 16 n)
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  Options.cli "icicle" (showVersion version) [] parser runCommand

parser :: Parser IcicleCommand
parser =
  Options.subparser . mconcat $ commands

commands :: [Mod CommandFields IcicleCommand]
commands = [
    Options.command'
      "repl"
      "Interactively evaluate icicle expressions."
      (IcicleRepl <$> pRepl)
  , Options.command'
      "compile"
      "Compile a dictionary to its C intermediate form."
      (IcicleCompile <$> pCompile)
  , Options.command'
      "query"
      "Run an icicle query over some data."
      (IcicleQuery <$> pQuery)
  , Options.command'
      "check"
      "Check an icicle dictionary for type errors."
      (IcicleCheck <$> pCheck)
  , Options.command'
      "lsp"
      "Run language server protocol"
      (IcicleLSP <$> pLSP)
  ]

pRepl :: Parser ReplOptions
pRepl =
  ReplOptions
    <$> pUseDotfiles
    <*> many pReplCommand
    <*> many (pReplZebra <|> pReplPSV)

pUseDotfiles :: Parser UseDotfiles
pUseDotfiles =
  Options.flag UseDotfiles SkipDotfiles $
    Options.long "skip-dotfiles" <>
    Options.help "Don't load the .icicle file from $HOME or the current directory"

pReplZebra :: Parser FilePath
pReplZebra =
  Options.argument Options.str $
    Options.metavar "INPUT_ZEBRA" <>
    Options.help "Path to a Zebra binary file to load" <>
    Options.action "file"

pReplPSV :: Parser FilePath
pReplPSV =
  Options.argument Options.str $
    Options.metavar "INPUT_PSV" <>
    Options.help "Path to a PSV file to load" <>
    Options.action "file"

pReplCommand :: Parser String
pReplCommand =
  Options.strOption $
    Options.long "init" <>
    Options.metavar "COMMAND" <>
    Options.help "A command to execute in the REPL before interative evaluation begins."

pCompile :: Parser Compile
pCompile =
  Compile
    <$> pure icicleFingerprint
    <*> pMaximumQueriesPerKernel
    <*> pInputDictionary
    <*> pOutputDictionarySea

pCheck :: Parser Check
pCheck =
  Check
    <$> pInputDictionary
    <*> pColourOutput

pMaximumQueriesPerKernel :: Parser MaximumQueriesPerKernel
pMaximumQueriesPerKernel =
  fmap MaximumQueriesPerKernel .
  Options.option Options.auto $
    Options.showDefault <>
    Options.value 100 <>
    Options.long "max-queries-per-kernel" <>
    Options.metavar "QUERY_COUNT" <>
    Options.help "The maximum number of queries to include in each compute kernel."


pInputDictionary :: Parser InputDictionary
pInputDictionary =
  pInputDictionaryIcicle

pInputDictionaryIcicle :: Parser InputDictionary
pInputDictionaryIcicle =
  fmap InputDictionarySorbet .
  Options.option Options.str $
    Options.long "input-icicle" <>
    Options.metavar "DICTIONARY_ICICLE" <>
    Options.help "Path to a ICICLE dictionary to compile." <>
    Options.action "file"

pOutputDictionarySea :: Parser OutputDictionarySea
pOutputDictionarySea =
  fmap OutputDictionarySea .
  Options.option Options.str $
    Options.long "output-c" <>
    Options.metavar "DICTIONARY_C" <>
    Options.help "Path to write the compiled C dictionary." <>
    Options.action "file"

pQuery :: Parser Query
pQuery =
  Query
    <$> pDictionarySea
    <*> pQueryInput
    <*> pQueryOutput
    <*> (pQuerySnapshot <|> pQueryChord)
    <*> pMaximumMapSize

pDictionarySea :: Parser DictionarySea
pDictionarySea =
  fmap DictionarySea .
  Options.option Options.str $
    Options.long "dictionary-c" <>
    Options.metavar "DICTIONARY_C" <>
    Options.help "Path to a compiled C dictionary."

pQueryInput :: Parser QueryInput
pQueryInput =
  fmap QueryInputZebra pInputZebra

pInputZebra :: Parser InputZebra
pInputZebra =
  fmap InputZebra .
  Options.option Options.str $
    Options.long "input-zebra" <>
    Options.metavar "INPUT_ZEBRA" <>
    Options.help "Path to a zebra binary file." <>
    Options.action "file"

pQueryOutput :: Parser QueryOutput
pQueryOutput =
      (QueryOutputZebra <$> pOutputZebra)
  <|> (QueryOutputPsv <$> pOutputPsv <*> optional pOutputPsvSchema)

pOutputZebra :: Parser OutputZebra
pOutputZebra =
  fmap OutputZebra .
  Options.option Options.str $
    Options.long "output-zebra" <>
    Options.metavar "OUTPUT_ZEBRA" <>
    Options.help "Path to write the zebra binary output file." <>
    Options.action "file"

pOutputPsv :: Parser OutputPsv
pOutputPsv =
  fmap OutputPsv $
  Options.option Options.str $
    Options.long "output-psv" <>
    Options.metavar "OUTPUT_PSV" <>
    Options.help "Path to write the dense psv file." <>
    Options.action "file"

pOutputPsvSchema :: Parser OutputPsvSchema
pOutputPsvSchema =
  fmap OutputPsvSchema .
  Options.option Options.str $
    Options.long "output-psv-schema" <>
    Options.metavar "OUTPUT_PSV_SCHEMA_JSON" <>
    Options.help "Location to write the output schema for a dense psv output. (default: <output-path>.schema.json)" <>
    Options.action "file"

pQuerySnapshot :: Parser QueryScope
pQuerySnapshot =
  fmap QuerySnapshot .
  Options.option (tryRead "cannot parse snapshot date" (dateOfText . Text.pack) id) $
    Options.long "snapshot" <>
    Options.metavar "SNAPSHOT_DATE" <>
    Options.help "Run a snapshot on an inclusive date."

pQueryChord :: Parser QueryScope
pQueryChord =
  fmap QueryChord .
  Options.option Options.str $
    Options.long "chord" <>
    Options.metavar "CHORD_DESCRIPTOR" <>
    Options.help "Path to a chord descriptor."

pMaximumMapSize :: Parser MaximumMapSize
pMaximumMapSize =
  fmap MaximumMapSize .
  Options.option Options.auto $
    Options.showDefault <>
    Options.value (1024 * 1024) <>
    Options.long "max-map-size" <>
    Options.metavar "QUERY_COUNT" <>
    Options.help "The maximum allowed size of a map at runtime."

pColourOutput :: Parser UseColor
pColourOutput =
  Options.flag NoColor UseColor $
    Options.long "colour"

pLSP :: Parser (Maybe FilePath)
pLSP =
  optional $
    Options.option Options.str $
      Options.long "debug-path" <>
      Options.metavar "PATH" <>
      Options.help "Prefix for debug files"


tryRead :: [Char] -> ([Char] -> Maybe a) -> (a -> b) -> Options.ReadM b
tryRead err f g =
  Options.readerAsk >>= \s ->
    case f s of
      Just i ->
        pure $ g i
      Nothing ->
        Options.readerError err

------------------------------------------------------------------------

icicleFingerprint :: Fingerprint
icicleFingerprint =
  Fingerprint $
    "icicle-" <> fromString (showVersion version)

runCommand :: IcicleCommand -> IO ()
runCommand = \case
  IcicleRepl options ->
    repl options

  IcicleCompile options ->
    orDie renderCompileError $
      icicleCompile options

  IcicleQuery options -> do
    orDie renderQueryError $ do
      icicleQuery options

  IcicleCheck dictionary -> do
    orDie renderCompileError $
      icicleCheck dictionary

  IcicleLSP path -> do
    runLSP path
