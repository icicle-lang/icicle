{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Parser (
    parseQueryTop
  , parseQuery
  , parseFactName
  , parseModule
  , prettyParse
  , Position
  -- , ParseError
  , Variable    (..)
  ) where

import Icicle.Sorbet.Abstract.Tokens (Variable (..))
import Icicle.Sorbet.Parse
import Icicle.Sorbet.Position

import Icicle.Source.Query

import Icicle.Data.Name

import Icicle.Internal.Pretty

import Data.Text

import P

import System.IO (FilePath)

parseModule :: FilePath -> Text -> Either ParseError (Module Position Variable)
parseModule source inp
 = sorbetModule source inp


parseQueryTop :: OutputId -> Text -> Either ParseError (QueryTop Position Variable)
parseQueryTop name inp
 = sorbet name inp

parseFactName :: Text -> Either ParseError UnresolvedInputId
parseFactName inp = sorbetUnresolvedInputId inp

parseQuery :: UnresolvedInputId -> OutputId -> Text -> Either ParseError (QueryTop Position Variable)
parseQuery v name inp
 = do q <- sorbetQuery inp
      return $ QueryTop v name q

prettyParse :: OutputId -> Text -> [Char]
prettyParse name inp
 = case parseQueryTop name inp of
    Left e -> "Error: " <> show e
    Right r -> show (pretty r)
