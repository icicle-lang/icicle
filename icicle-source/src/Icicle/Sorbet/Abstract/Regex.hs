{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Icicle.Sorbet.Abstract.Regex (
  parser
) where

import            Control.Monad.Combinators.Expr
import            Data.Void
import            Data.These

import            Text.Megaparsec as Mega
import            Text.Megaparsec.Char as Mega

import            Icicle.Data.Regex (Regex)
import qualified  Icicle.Data.Regex as Regex

import            P
import qualified  Prelude as Savage


type Parser = Parsec Void Text


parser :: Parser Regex
parser = do
  body <- term
  let
    dotStar =
      Regex.star Regex.dot
  return $
    dotStar `Regex.times` body `Regex.times` dotStar


term :: Parser Regex
term = makeExprParser atom ops  where
  ops = [ [ Postfix (Regex.star <$ single '*')
          , Postfix (Regex.plus <$ single '+')
          , Postfix (Regex.question <$ single '?')
          , Postfix (Regex.bound <$> nBound)
          ]
        , [ InfixR (return Regex.times) ]
        , [ InfixR (Regex.add <$ single '|') ]
        ]

  atom = msum [ Regex.dot <$ single '.'
              , Regex.once <$ single '\\' <*> oneOf special
              , Regex.once <$> lit
              , parens term
              ]

  special = ['.', '*', '+', '?', '|', '(', ')', '^', '$']
  lit = noneOf special
  parens = between (single '(') (single ')')



  nBound :: Parser (These Int Int)
  nBound = between (single '{') (single '}') (a <|> b)
    where
      a = do
        a'     <- Savage.read <$> Mega.some Mega.digitChar
        mComma <- optional (single ',')
        case mComma of
          Nothing ->
            return (These a' a')
          Just _ -> do
            o <- optional $ Savage.read <$> Mega.some Mega.digitChar
            case o of
              Nothing ->
                return (This a')
              Just b' ->
                return (These a' b')
      b = do
        _  <- single ','
        b' <- Savage.read <$> Mega.some Mega.digitChar
        return (That b')
