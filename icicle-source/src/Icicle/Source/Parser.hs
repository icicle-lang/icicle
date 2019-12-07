{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Parser (
    parseQueryTop
  , parseQuery
  , parseFactName
  , parseFunctions
  , prettyParse
  , Position
  -- , ParseError
  , Variable    (..)
  ) where

import Icicle.Sorbet.Abstract.Tokens (Variable (..))
import Icicle.Sorbet.Abstract.Parser (Decl (..))
import Icicle.Sorbet.Parse
import Icicle.Sorbet.Position

import Icicle.Source.Query

import Icicle.Common.Base

import Icicle.Data.Name

import Icicle.Internal.Pretty
import Text.Parsec (SourcePos)

import Data.Text

import P

import System.IO (FilePath)

parseFunctions :: FilePath -> Text -> Either ParseError [((SourcePos, Name Variable), (Exp SourcePos Variable))]
parseFunctions source inp
 = do decls <- sorbetFunctions source inp
      return [((toParsec p, n), reannot toParsec x) | DeclFun p n x <- decls]

parseQueryTop :: OutputId -> Text -> Either ParseError (QueryTop SourcePos Variable)
parseQueryTop name inp
 = reannot toParsec <$> sorbet name inp

parseFactName :: Text -> Either ParseError UnresolvedInputId
parseFactName inp = undefined

parseQuery :: UnresolvedInputId -> OutputId -> Text -> Either ParseError (QueryTop SourcePos Variable)
parseQuery v name inp = undefined

prettyParse :: OutputId -> Text -> [Char]
prettyParse name inp
 = case parseQueryTop name inp of
    Left e -> "Error: " <> show e
    Right r -> show (pretty r)
