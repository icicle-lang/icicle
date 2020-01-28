{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Parser for the Icicle Source language from the Sorbet lexer.
--
--   There were two ways of going about this, writing an elaborator
--   from the Sorbet Concrete parse tree, or writing a new AST parser.
--
--   Having a look at the Sorbet Concrete Parser, it looked to me
--   like a few very important things were missing, and the Concrete
--   syntax of Sorbet is quite different to the original AST of
--   Icicle.
--
--   Also, Sorbet is quite a bit richer, and I can't actually support
--   all of it as written at the moment.
--
--   I decided to go down this route, as I can share as much as I
--   need from both of the current sources, and can incrementally
--   update the Icicle AST and new parser to get closer to Sorbet
--   over time.
module Icicle.Storage.Dictionary.Sorbet.Parser (
    pDictionary
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.String (String, unlines)
import           Data.Scientific (toRealFloat)

import           Icicle.Sorbet.Abstract.Parser
import           Icicle.Sorbet.Abstract.Tokens
import           Icicle.Sorbet.Abstract.Type

import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           Icicle.Common.Base as Common
import           Icicle.Data.Name
import           Icicle.Data.Time (Date (..), midnight)
import           Icicle.Source.Query
import           Icicle.Source.Type
import           Icicle.Source.Parser.Constructor (checkPat, constructors)
import           Icicle.Source.Parser.Operators

import           Icicle.Storage.Dictionary.Data


import           P

import           Text.Megaparsec (choice)
import qualified Text.Megaparsec as Mega

type Var = Variable


pDictionary :: Parser s m => m (DictionaryConfig, [DictionaryInput'], [DictionaryOutput'])
pDictionary = do
  name    <- pDictionaryName

  let
    namespace = do
      (c,cs) <- Text.uncons name
      parseNamespace $ Text.cons (Char.toLower c) cs

  nspace  <- maybe (fail "Invalid namespace") pure namespace
  _       <- pToken Tok_LBrace
  imports <- pImport `Mega.sepEndBy` pToken Tok_Semi
  ios     <- (fmap Left (pInput nspace) <|> fmap Right (pOutput nspace)) `Mega.sepEndBy` pToken Tok_Semi
  _       <- pToken Tok_RBrace

  let
    config =
      DictionaryConfig (Just name) Nothing Nothing Nothing imports []

    (inputs, outputs) =
      partitionEithers ios

  pure $
    (config, inputs, outputs)



pDictionaryName :: Parser s m => m Text
pDictionaryName = do
  _ <- pToken Tok_Dictionary
  n <- pConstructorText
  _ <- pToken Tok_Where
  return n



pConstructorText :: Parser s m => m Text
pConstructorText = do
  (_, Construct x) <- pConId
  return x


pInputName :: Parser s m => m InputName
pInputName = do
  (_, Variable x) <- pVarId
  maybe (fail "Couldn't parse as InputName") pure (parseInputName x)



pOutputName :: Parser s m => m OutputName
pOutputName = do
  (_, Variable x) <- pVarId
  maybe (fail "Couldn't parse as InputName") pure (parseOutputName x)



pImport :: Parser s m => m Text
pImport = do
  _      <- pToken Tok_Import
  (_, s) <- pString
  return s



pInput :: Parser s m => Namespace -> m DictionaryInput'
pInput ns = do
  _      <- pToken Tok_Input
  n      <- pInputName
  _      <- pToken Tok_Colon
  vt     <- pPositionedFail pType (valTypeOfType . snd) $ unlines [
              "Input streams must be serialisable types."
            , ""
            , "Function types, modalities, and type variables are not supported."
            ]

  pure $
    DictionaryInput' (InputId ns n) vt Nothing (ConcreteKey' Nothing)


pPositionedFail :: Parser s m => m a -> (a -> Maybe b) -> String -> m b
pPositionedFail x check err = do
  o <- Mega.getOffset
  v <- x
  maybe (Mega.setOffset o >> fail err) pure (check v)


pOutput :: Parser s m => Namespace -> m DictionaryOutput'
pOutput ns = do
  _      <- pToken Tok_Feature
  n      <- pOutputName
  _      <- pToken Tok_Equals
  t      <- pTop (OutputId ns n)

  pure $
    DictionaryOutput' (OutputId ns n) (reannot toParsec t)

