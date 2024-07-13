{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Icicle.Sorbet.Abstract.Tokens (
    Parser
  , ParserBundle
  , Variable (..)
  , Operator (..)
  , Construct (..)

  , pVariable
  , pVarId
  , pVarOp
  , pConId

  , pInteger
  , pRational
  , pString
  , pDate

  , pToken
  , tryToken
  , tryPosToken
  , tryPositioned
  , (<%>)
  , (<?>)
  , sepBy1
  , position

  , failAtOffset
  , pPositionedFail
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Scientific (Scientific)
import qualified Data.Set as Set
import           Data.String (String)
import qualified Data.Text as T
import           Data.Thyme (Day)
import           Data.Void (Void)

import           GHC.Generics (Generic)

import           Icicle.Common.Base

import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           Icicle.Source.Lexer.Token (Variable (..), Operator (..))

import           P

import           Text.Megaparsec (label)
import qualified Text.Megaparsec as Mega
import           Text.Megaparsec (MonadParsec, TraversableStream)


type Parser s m =
  (MonadParsec Void s m, Mega.Token s ~ Positioned Token, MonadFail m, TraversableStream s)

type ParserBundle =
  Mega.ParseErrorBundle (PositionedStream Token) Void

newtype Construct =
  Construct Text
  deriving (Eq, Ord, Show, Generic)

pVariable :: Parser s m => m (Range, Name Variable)
pVariable = do
  (p, vid) <- pVarId
  pure (p, nameOf (NameBase vid))


pVarId :: Parser s m => m (Range, Variable)
pVarId =
  label "variable" .
  tryToken $ \pos -> \case
    Tok_VarId varId ->
      Just (pos, Variable varId)
    Tok_Wild ->
      Just (pos, Variable "_")
    _ ->
      Nothing

pVarOp :: Parser s m => m (Range, Operator)
pVarOp =
  label "operator" .
  tryToken $ \pos -> \case
    Tok_VarOp varOp ->
      Just (pos, Operator varOp)
    _ ->
      Nothing

pConId :: Parser s m => m (Range, Construct)
pConId =
  label "constructor" .
  tryToken $ \pos -> \case
    Tok_ConId conId ->
      Just (pos, Construct conId)
    _ ->
      Nothing

pInteger :: Parser s m => m (Range, Integer)
pInteger =
  label "integer literal" .
  tryPosToken $ \case
    Tok_Integer integer ->
      Just integer
    _ ->
      Nothing

pRational :: Parser s m => m (Range, Scientific)
pRational =
  label "rational literal" .
  tryPosToken $ \case
    Tok_Rational rational ->
      Just rational
    _ ->
      Nothing

pString :: Parser s m => m (Range, Text)
pString =
  label "string literal" .
  tryPosToken $ \case
    Tok_String string ->
      Just string
    _ ->
      Nothing

pDate :: Parser s m => m (Range, Day)
pDate =
  label "date literal" .
  tryPosToken $ \case
    Tok_Date date ->
      Just date
    _ ->
      Nothing


pToken :: Parser s m => Token -> m Range
pToken tok0 =
  label ("“" <> T.unpack (renderToken tok0) <> "”") .
  tryToken $ \pos tok ->
    if tok0 == tok then
      Just pos
    else
      Nothing

tryToken :: Parser s m => (Range -> Token -> Maybe a) -> m a
tryToken f  =
  tryPositioned $ \(Positioned start end tok) ->
    f (Range start end) tok

tryPosToken :: Parser s m => (Token -> Maybe a) -> m (Range, a)
tryPosToken f  =
  tryPositioned $ \(Positioned start end tok) ->
    (Range start end,) <$> f tok

tryPositioned :: Parser s m => (Positioned Token -> Maybe a) -> m a
tryPositioned f =
  let
    errFor =
      Set.singleton (Mega.Label $ 't' :| "ry positioned")

  in
    Mega.token f errFor

--
-- Like liftA2 but allows 'a' to be derived from 'b'.
--
-- Used for extracting the annotation (aka the source position) from the first
-- real constructor argument and using it as the annotation for the
-- constructor.
--
(<%>) :: Monad m => (a -> b -> c) -> (b -> a, m b) -> m c
(<%>) f (g, m) = do
  b <- m
  return $
    f (g b) b

infixl 4 <%>


(<?>) :: Parser s m => m a -> String -> m a
(<?>) = flip label


sepBy1 :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepBy1 p sep =
  (:|) <$> p <*> many (sep *> p)

position :: Parser s m => m Position
position = do
  Mega.SourcePos file line col <- Mega.getSourcePos
  pure $
    Position file
      (fromIntegral $ Mega.unPos line)
      (fromIntegral $ Mega.unPos col)


failAtOffset :: Parser s m => Int -> String -> m b
failAtOffset offset errMsg =
  Mega.region (setErrorOffset offset) (fail errMsg)


pPositionedFail :: Parser s m => m a -> (a -> Maybe b) -> String -> m b
pPositionedFail x check err = do
  offset <- Mega.getOffset
  Mega.region (setErrorOffset offset) $ do
    v <- x
    maybe (fail err) pure (check v)


setErrorOffset :: Int -> Mega.ParseError s e ->  Mega.ParseError s e
setErrorOffset o (Mega.TrivialError _ a b) = Mega.TrivialError o a b
setErrorOffset o (Mega.FancyError   _ a) = Mega.FancyError   o a
