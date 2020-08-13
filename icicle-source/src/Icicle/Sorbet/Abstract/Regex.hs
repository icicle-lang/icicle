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

import            Text.Megaparsec as Mega
import            Text.Megaparsec.Char as Mega

import            Icicle.Data.Regex (Regex)
import qualified  Icicle.Data.Regex as Regex

import            P


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
          , Postfix ((`Regex.add` Regex.epsilon) <$ single '?')
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

  nOf :: Parser Char
  nOf = between (single '{') (single '}') Mega.digitChar
