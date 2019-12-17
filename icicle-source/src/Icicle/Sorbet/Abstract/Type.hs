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
module Icicle.Sorbet.Abstract.Type (
    pConstrainedType
  ) where

import qualified Data.List as List
import           Data.Scientific (toRealFloat)
import           Data.String (String)

import           Icicle.Sorbet.Abstract.Tokens

import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           Icicle.Common.Base as Common
import           Icicle.Data.Name
import           Icicle.Data.Time (Date (..), midnight)
import           Icicle.Source.Query
import           Icicle.Source.Type
import           Icicle.Source.Parser.Constructor (checkPat, constructors)
import           Icicle.Source.Parser.Operators
import           Icicle.Source.Checker.Function (Decl (..))

import           P

import           Text.Megaparsec (try, choice, label)
import qualified Text.Megaparsec as Mega


type Var = Variable


pConstrainedType :: Parser s m => m (Type Var)
pConstrainedType = do
  (_, constraints) <- List.unzip <$> many (try pConstraint)
  (_, typ)         <- pType
  let
    freeVars =
      toList (freeT typ)

    final =
      if null freeVars && null constraints then
        typ
      else
        TypeForall freeVars constraints typ

  return final


pConstraint :: Parser s m => m (Position, Constraint Var)
pConstraint =
  label "constraint" $
  pConstraintSingle <* pToken Tok_RArrowEquals


pType :: Parser s m => m (Position, Type Var)
pType =
  label "type" $ do
    p  <- position
    xs <- some ((Left <$> pTypeSimple) <|> (Right <$> pTypeOperator)) <?> "expression"
    either fail (\t -> return (p, t)) (defixType xs)


 where


pTypeSimple :: Parser s m => m (Position, Type Var)
pTypeSimple =
  choice [
      pTypeSingle
    , pTypeVar
    , pTypeNested
    ]


-- We only support two infix operators for types
-- and they both have the same precedence but with
-- opposite fixity. So we don't technically need
-- to use a shunting algorithm, as they can't be
-- interspersed.
data TypeOperator
  = OpFunctionArrow
  | OpComma
  deriving (Eq)


pTypeOperator :: Parser s m => m TypeOperator
pTypeOperator =
      OpComma         <$ pToken Tok_Comma
  <|> OpFunctionArrow <$ pToken Tok_RArrowDash


pTypeSingle :: Parser s m => m (Position, Type Var)
pTypeSingle =
  label "type constructor" $ do
    (p, Construct n) <- pConId
    case List.lookup n simpleTypes of
      Just c  -> do rest <- c; return (p, rest)
      Nothing -> fail ("Not a type constructor: " <> show n)


pTypeVar :: Parser s m => m (Position, Type Var)
pTypeVar =
  label "type variable" $
  second TypeVar
    <$> pVariable


pTypeNested :: Parser s m => m (Position, Type Var)
pTypeNested =
  label "nested type" $
    pToken Tok_LParen *> pType <* pToken Tok_RParen


-- pTypeTuple :: Parser s m => m (Type Var)
-- pTypeTuple =
--   label "type tuple" $
--   TypeTuple
--     <$> pToken Tok_LParen
--     <*> (pType `sepBy` pToken Tok_Comma) <* pToken Tok_RParen


simpleTypes :: Parser s m => [(Text, m (Type Var))]
simpleTypes
   = [("Bool",                    pure $ BoolT)
     ,("Int",                     pure $ IntT)
     ,("Double",                  pure $ DoubleT)
     ,("Unit",                    pure $ UnitT)
     ,("ErrorT",                  pure $ ErrorT)
     ,("Time",                    pure $ TimeT)
     ,("String",                  pure $ StringT)
     ,("Option",                  OptionT <$> (snd <$> pTypeSimple))
     ,("Sum",                     SumT    <$> (snd <$> pTypeSimple) <*> (snd <$> pTypeSimple))
     ,("Array",                   ArrayT  <$> (snd <$> pTypeSimple))
     ,("Group",                   GroupT  <$> (snd <$> pTypeSimple) <*> (snd <$> pTypeSimple))
     ,("Pure",                    Temporality TemporalityPure       <$> (snd <$> pTypeSimple))
     ,("Element",                 Temporality TemporalityElement    <$> (snd <$> pTypeSimple))
     ,("Aggregate",               Temporality TemporalityAggregate  <$> (snd <$> pTypeSimple))
     ,("Possibly",                Possibility PossibilityPossibly   <$> (snd <$> pTypeSimple))
     ,("Definitely",              Possibility PossibilityDefinitely <$> (snd <$> pTypeSimple))
     ,("Possibility",             Possibility <$> (snd <$> pTypeSimple) <*> (snd <$> pTypeSimple))
     ]


pConstraintSingle :: Parser s m => m (Position, Constraint Var)
pConstraintSingle =
  pConstraintSimple <|> pEqualityConstraint


pConstraintSimple :: Parser s m => m (Position, Constraint Var)
pConstraintSimple =
  label "type constraint" $ do
    (p, Construct n) <- pConId
    case List.lookup n simpleConstraints of
      Just c  -> do rest <- c; return (p, rest)
      Nothing -> fail ("Not a type constructor: " <> show n)


simpleConstraints :: Parser s m => [(Text, m (Constraint Var))]
simpleConstraints
   = [("Num",                  CIsNum <$> (snd <$> pTypeSimple))
     ]


pEqualityConstraint :: Parser s m => m (Position, Constraint Var)
pEqualityConstraint =
  label "equality constraint" $ do
    (p, ret)         <- pTypeSimple
    _                <- pToken Tok_EqualsColon
    (_, Construct n) <- pConId
    case List.lookup n simpleEqualityConstraints of
      Just c  -> do rest <- c ret; return (p, rest)
      Nothing -> fail ("Not an equality constraint: " <> show n)


simpleEqualityConstraints :: Parser s m => [(Text, Type Var -> m (Constraint Var))]
simpleEqualityConstraints
   = [("PossibilityOfNum",        \ret -> CPossibilityOfNum ret <$> (snd <$> pTypeSimple))
     ]

-- | FIX
--
--   Generalise and use the proper shunting algorithm from Source.
defixType :: [Either (a, Type n) TypeOperator] -> Either String (Type n)
defixType = \case
  [] ->
    Left "Can't parse nothing as type"
  (Right op : _) ->
    Left "Can't parse prefix type operator"
  (Left (_, typ) : []) ->
    Right typ
  (Left _ : Left _ : _) ->
      Left "Over application of type"
  (Left (_, typ) : Right op : rest) -> do
    typs <- pullSimilar op (Right op : rest)
    return $ goOp op (typ : typs)

 where
  pullSimilar op (Right op' : Left (_, typ) : rest) | op == op' = do
    further <- pullSimilar op rest
    return (typ : further)
  pullSimilar op [] =
    return []
  pullSimilar op _ =
    Left "Can't mix infix type operators with same precedence or over apply types"
  goOp OpComma xs = List.foldl1 PairT xs
  goOp OpFunctionArrow xs = List.foldr1 TypeArrow xs
