{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl.Parser (
    parseCommand

  , pCommand

  , pBlank
  , pH
  , pHelp

  , pSet
  , pSetOption
  , pSetSnapshot
  , pSetMaxMapSize
  , pSetFlagOn
  , pSetFlagOff
  , pFlag

  , pDate
  , pLexeme
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.String (String)
import           Data.Void (Void)

import           Icicle.Data.Time (Date, dateOfYMD)
import           Icicle.Repl.Data
import           Icicle.Repl.Flag

import           P

import           System.IO (FilePath)

import           Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Char as Mega
import           Text.Printf (printf)

type Parser = Mega.Parsec Void String

pLexeme :: Parser a -> Parser a
pLexeme =
  Lexer.lexeme Mega.space

pBlank :: Parser Command
pBlank =
  CommandBlank <$ Mega.space

pH :: Parser Command
pH =
  CommandHelp <$ pLexeme (Mega.string ":h")

pHelp :: Parser Command
pHelp =
  CommandHelp <$ pLexeme (Mega.string ":help")

pFlag :: Parser Flag
pFlag = do
  name <- pLexeme $ some (Mega.alphaNumChar <|> Mega.char' '-')
  case Map.lookup name namedFlags of
    Nothing ->
      fail $ "Unknown flag: " <> name
    Just flag ->
      pure flag

pSetFlagOn :: Parser SetOption
pSetFlagOn =
  SetFlagOn <$> (Mega.char '+' *> pFlag)

pSetFlagOff :: Parser SetOption
pSetFlagOff =
  SetFlagOff <$> (Mega.char '-' *> pFlag)

pDate :: Parser Date
pDate =
  pLexeme $ do
    year <- Lexer.decimal <?> "year"
    _ <- Mega.char '-'
    month <- Lexer.decimal <?> "month"
    _ <- Mega.char '-'
    day <- Lexer.decimal <?> "day"

    case dateOfYMD year month day of
      Nothing ->
        fail $ printf "Not a valid gregorian calendar date: %04d-%02d-%02d"  year month day
      Just x ->
        pure x

pSetSnapshot :: Parser SetOption
pSetSnapshot = do
  _ <- pLexeme $ Mega.string "snapshot"
  SetSnapshot <$> pDate <?> "snapshot date"

pSetMaxMapSize :: Parser SetOption
pSetMaxMapSize = do
  _ <- pLexeme $ Mega.string "max-map-size"
  SetMaxMapSize
    <$> pLexeme Lexer.decimal
    <?> "integer value for maximum map size"

pSetLimit :: Parser SetOption
pSetLimit = do
  _ <- pLexeme $ Mega.string "limit"
  SetLimit
    <$> pLexeme Lexer.decimal
    <?> "integer value for output limit"

pSetCFlags :: Parser SetOption
pSetCFlags = do
  _ <- pLexeme $ Mega.string "cflags"
  SetCFlags
    <$> Mega.manyTill Mega.anySingle Mega.eol

pSetFlag :: Parser SetOption
pSetFlag =
  Mega.choice [
      pSetFlagOn
    , pSetFlagOff
    ]

pSetOption :: Parser SetOption
pSetOption =
  Mega.choice [
      pSetSnapshot
    , pSetMaxMapSize
    , pSetLimit
    , pSetCFlags
    ]

pSet :: Parser Command
pSet = do
  _ <- pLexeme $ Mega.string ":set"
  Mega.choice [
      CommandSet . pure <$> pSetOption
    , CommandSet <$> some pSetFlag
    , CommandSet <$> pure [SetShowOptions]
    ]

pDictionary :: Parser Command
pDictionary = do
  CommandDictionary <$ pLexeme (Mega.string ":dictionary")

pFilePath :: Parser FilePath
pFilePath =
  pLexeme . Mega.some $ Mega.satisfy (not . Char.isSpace)

pLoad :: Parser Command
pLoad = do
  _ <- pLexeme $ Mega.string ":load"
  CommandLoad <$> pFilePath <?> "path to dictionary, data or icicle functions"

pQuery :: Parser Command
pQuery =
  CommandQuery <$> Mega.some Mega.anySingle

pLet :: Parser Command
pLet = do
  _ <- pLexeme $ Mega.string ":let"
  pos <- Mega.getSourcePos

  let
    padding =
      List.replicate (fromIntegral . Mega.unPos $ Mega.sourceColumn pos) ' '

  CommandLet . (padding <>) <$> Mega.someTill Mega.anySingle Mega.eol

pComment :: Parser Command
pComment =
  fmap (CommandComment . ("--" <>)) $
    Mega.string "--" *> Mega.manyTill Mega.anySingle Mega.eol

pCommand :: Parser Command
pCommand =
  Mega.choice [
      Mega.try pHelp
    , pH
    , pSet
    , pDictionary
    , pLoad
    , pLet
    , pComment
    , pQuery
    , pBlank
    ]

parseCommand :: String -> Either String Command
parseCommand line =
  first (Mega.errorBundlePretty) $
    Mega.parse (Mega.space *> pCommand <* Mega.eof) "<interactive>" (line <> "\n")
