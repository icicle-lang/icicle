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
  , sorbetModule
  , sorbetType
  , sorbetUnresolvedInputId

  , ParseError (..)
  , positionedParseError
  ) where

import           Data.String (String)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text

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

import qualified Text.Megaparsec as Mega
import           Text.Megaparsec (MonadParsec)



sorbetModule :: String -> Text -> Either ParseError (Module Position Variable)
sorbetModule source input = do
  lexed  <- first LexParseError $ Mega.runParser (consumeAll lexProgram) source input
  layed  <- first LayoutParseError $ layoutProgram lexed
  parsed <- first AbstractParseError $ Mega.runParser (consumeAll pModule) source (PositionedStream input layed)
  return parsed


sorbet :: OutputId -> Text -> Either ParseError (QueryTop Position Variable)
sorbet oid input = do
  lexed  <- first LexParseError $ Mega.runParser (consumeAll lexProgram) "" input
  layed  <- first LayoutParseError $ layoutRepl lexed
  parsed <- first AbstractParseError $ Mega.runParser (consumeAll (pTop oid)) "" (PositionedStream input layed)
  return parsed



sorbetType :: Text -> Either ParseError (Scheme Variable)
sorbetType input = do
  lexed  <- first LexParseError $ Mega.runParser (consumeAll lexProgram) "" input
  parsed <- first AbstractParseError $ Mega.runParser (consumeAll (pTypeScheme)) "" (PositionedStream input lexed)
  return parsed


sorbetQuery :: Text -> Either ParseError (Query Position Variable)
sorbetQuery input = do
  lexed  <- first LexParseError $ Mega.runParser (consumeAll lexProgram) "" input
  layed  <- first LayoutParseError $ layoutProgram lexed
  parsed <- first AbstractParseError $ Mega.runParser (consumeAll pQuery) "" (PositionedStream input layed)
  return parsed


sorbetUnresolvedInputId :: Text -> Either ParseError UnresolvedInputId
sorbetUnresolvedInputId input = do
  lexed  <- first LexParseError $ Mega.runParser (consumeAll lexProgram) "" input
  first AbstractParseError $ Mega.runParser (consumeAll pUnresolvedInputId) "" (PositionedStream input lexed)

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


positionedParseError :: ParseError -> NonEmpty (String, String, Position)
positionedParseError = \case
  LexParseError bundle ->
    let
      (withPositions, _) =
        Mega.attachSourcePos Mega.errorOffset (Mega.bundleErrors bundle) (Mega.bundlePosState bundle)

    in
      fmap (\(e,p) -> ("Lexer", Mega.parseErrorTextPretty e, fromSourcePos p)) withPositions

  LayoutParseError le@(UnexpectedEndOfScope _ _ (Positioned s _ _)) ->
    ("Layout", Text.unpack (renderLayoutError le), s) :| []

  AbstractParseError bundle ->
    let
      (withPositions, _) =
        Mega.attachSourcePos Mega.errorOffset (Mega.bundleErrors bundle) (Mega.bundlePosState bundle)

    in
      fmap (\(e,p) -> ("Parser", Mega.parseErrorTextPretty e, fromSourcePos p)) withPositions
