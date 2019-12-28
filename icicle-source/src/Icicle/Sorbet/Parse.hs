{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Parser for the Icicle Source language from the Sorbet lexer.
module Icicle.Sorbet.Parse (
    sorbet
  , sorbetQuery
  , sorbetFunctions
  , sorbetType

  , ParseError
  , annotOfParseError
  ) where

import           Data.String (String)

import           Icicle.Sorbet.Abstract.Parser
import           Icicle.Sorbet.Abstract.Tokens
import           Icicle.Sorbet.Abstract.Type
import           Icicle.Sorbet.Lexical.Lexer
import           Icicle.Sorbet.Lexical.Layout
import           Icicle.Sorbet.Position

import           Icicle.Internal.Pretty (Pretty (..))
import           Icicle.Source.Query
import           Icicle.Source.Type

import           Icicle.Data.Name

import           P

import qualified Text.Parsec.Pos as Parsec

import qualified Text.Megaparsec as Mega
import           Text.Megaparsec (MonadParsec)



sorbetFunctions :: String -> Text -> Either ParseError [Decl Position Variable]
sorbetFunctions source input = do
  lexed  <- first LexParseError $ Mega.runParser (consumeAll lexProgram) source input
  layed  <- first LayoutParseError $ layoutProgram lexed
  parsed <- first AbstractParseError $ Mega.runParser (consumeAll pDecls) source (PositionedStream input layed)
  return (snd parsed)


sorbet :: OutputId -> Text -> Either ParseError (QueryTop Position Variable)
sorbet oid input = do
  lexed  <- first LexParseError $ Mega.runParser (consumeAll lexProgram) "" input
  layed  <- first LayoutParseError $ layoutProgram lexed
  parsed <- first AbstractParseError $ Mega.runParser (consumeAll (pTop oid)) "" (PositionedStream input layed)
  return parsed



sorbetType :: Text -> Either ParseError (Scheme Variable)
sorbetType input = do
  lexed  <- first LexParseError $ Mega.runParser (consumeAll lexProgram) "" input
  parsed <- first AbstractParseError $ Mega.runParser (consumeAll (pConstrainedType)) "" (PositionedStream input lexed)
  return parsed


sorbetQuery :: Text -> Either ParseError (Query Position Variable)
sorbetQuery input = do
  lexed  <- first LexParseError $ Mega.runParser (consumeAll lexProgram) "" input
  layed  <- first LayoutParseError $ layoutProgram lexed
  parsed <- first AbstractParseError $ Mega.runParser (consumeAll pQuery) "" (PositionedStream input layed)
  return parsed


consumeAll :: MonadParsec e s m => m a ->  m a
consumeAll f = do
 r <- f
 Mega.eof
 return r


data ParseError
  = LexParseError (LexerBundle Text)
  | LayoutParseError LayoutError
  | AbstractParseError ParserBundle
  deriving (Eq, Show)


instance Pretty ParseError where
  pretty = \case
    LexParseError bundle ->
      pretty (Mega.errorBundlePretty bundle)

    LayoutParseError lerr ->
      pretty (renderLayoutError lerr)

    AbstractParseError bundle ->
      pretty (Mega.errorBundlePretty bundle)


annotOfParseError :: ParseError -> Maybe Parsec.SourcePos
annotOfParseError = \case
  LayoutParseError (UnexpectedEndOfScope _ _ (Positioned s _ _)) ->
    Just (toParsec s)
  _ ->
    Nothing
