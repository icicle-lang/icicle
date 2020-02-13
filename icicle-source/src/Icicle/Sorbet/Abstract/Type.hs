{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Parser for the Icicle Source language Types from the Sorbet Lexer.
--
module Icicle.Sorbet.Abstract.Type (
    pTypeScheme
  , pType
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.String (String)

import           Icicle.Sorbet.Abstract.Tokens

import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           Icicle.Common.Base as Common
import           Icicle.Source.Type

import           P

import           Text.Megaparsec (try, choice, label)
import qualified Text.Megaparsec as Mega


type Var = Variable


pTypeScheme :: Parser s m => m (Scheme Var)
pTypeScheme = do
  (_, constraints) <- List.unzip <$> many (try pConstraint)
  (_, typ)         <- pType
  let
    freeVars =
      toList (freeT typ)

    ret =
      Forall freeVars constraints typ

  return ret


pConstraint :: Parser s m => m (Position, Constraint Var)
pConstraint =
  label "constraint" $
  pConstraintSingle <* pToken Tok_RArrowEquals


pType :: Parser s m => m (Position, Type Var)
pType =
  label "type" $ do
    o  <- Mega.getOffset
    p  <- position
    xs <- some ((Left <$> pTypeSimple) <|> (Right <$> pTypeOperator)) <?> "expression"
    either (failAtOffset o) (\t -> return (p, t)) (defixType xs)


pTypeSimple :: Parser s m => m (Position, Type Var)
pTypeSimple =
  choice [
      pTypeSingle
    , pTypeVar
    , pTypeNested
    , pTypeRecord
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
    o                <- Mega.getOffset
    (p, Construct n) <- pConId
    case List.lookup n simpleTypes of
      Just c  -> do rest <- c; return (p, rest)
      Nothing -> failAtOffset o ("Not a type constructor: " <> show n)


pTypeVar :: Parser s m => m (Position, Type Var)
pTypeVar =
  label "type variable" $
  second TypeVar
    <$> pVariable


pTypeNested :: Parser s m => m (Position, Type Var)
pTypeNested =
  label "nested type" $
    pToken Tok_LParen *> pType <* pToken Tok_RParen


pTypeRecord :: Parser s m => m (Position, Type Var)
pTypeRecord =
  label "record type" $ do
    p <- pToken Tok_LBrace
    let
      asField (Variable x) = Common.StructField x

      pField = do
        (_, v) <- pVarId
        _      <- pToken Tok_Colon
        (_, t) <- pTypeSimple
        return (asField v, t)

    fields <- pField `Mega.sepBy` pToken Tok_Comma

    _ <- pToken Tok_RBrace
    return (p, StructT $ Map.fromList fields)



simpleTypes :: Parser s m => [(Text, m (Type Var))]
simpleTypes
   = [("Bool",                    pure $ BoolT)
     ,("Int",                     pure $ IntT)
     ,("Double",                  pure $ DoubleT)
     ,("Unit",                    pure $ UnitT)
     ,("ErrorT",                  pure $ ErrorT)
     ,("Time",                    pure $ TimeT)
     ,("String",                  pure $ StringT)
     ,("Option",                  one  $ OptionT)
     ,("Sum",                     two  $ SumT)
     ,("Array",                   one  $ ArrayT)
     ,("Group",                   two  $ GroupT)
     ,("Pure",                    one  $ Temporality TemporalityPure)
     ,("Element",                 one  $ Temporality TemporalityElement)
     ,("Aggregate",               one  $ Temporality TemporalityAggregate)
     ,("Temporality",             two  $ Temporality)
     ,("Possibly",                one  $ Possibility PossibilityPossibly)
     ,("Definitely",              one  $ Possibility PossibilityDefinitely)
     ,("Possibility",             two  $ Possibility)
     ]


pConstraintSingle :: Parser s m => m (Position, Constraint Var)
pConstraintSingle =
  pConstraintSimple <|> pEqualityConstraint


pConstraintSimple :: Parser s m => m (Position, Constraint Var)
pConstraintSimple =
  label "type constraint" $ do
    o  <- Mega.getOffset
    (p, Construct n) <- pConId
    case List.lookup n simpleConstraints of
      Just c  -> do rest <- c; return (p, rest)
      Nothing -> failAtOffset o ("Not a type constructor: " <> show n)


simpleConstraints :: Parser s m => [(Text, m (Constraint Var))]
simpleConstraints
   = [("Num",                  one CIsNum)
     ]


pEqualityConstraint :: Parser s m => m (Position, Constraint Var)
pEqualityConstraint =
  label "equality constraint" $ do
    o                <- Mega.getOffset
    (p, ret)         <- pTypeSimple
    _                <- pToken Tok_EqualsColon
    (_, Construct n) <- pConId
    case List.lookup n simpleEqualityConstraints of
      Just c  -> do rest <- c ret; return (p, rest)
      Nothing -> failAtOffset o ("Not an equality constraint: " <> show n)


simpleEqualityConstraints :: Parser s m => [(Text, Type Var -> m (Constraint Var))]
simpleEqualityConstraints
   = [("PossibilityOfNum",        one   . CPossibilityOfNum)
     ,("TemporalityJoin",         two   . CTemporalityJoin)
     ,("ReturnOfLet",             two   . CReturnOfLetTemporalities)
     ,("DataOfLatest",            three . CDataOfLatest)
     ,("PossibilityOfLatest",     two   . CPossibilityOfLatest)
     ,("PossibilityJoin",         two   . CPossibilityJoin)
     ,("HasField",                  \k -> CHasField k <$> (snd <$> pTypeSimple) <*> pField)
     ]
  where
    pField :: Parser s m => m StructField
    pField =
      label "field" .
      tryToken $ \_ -> \case
        Tok_VarId fieldId ->
          Just (StructField fieldId)
        _ ->
          Nothing

-- | Shunt infix type operators to a type.
--
--   Generalise and use the proper shunting algorithm from Source.
defixType :: [Either (a, Type n) TypeOperator] -> Either String (Type n)
defixType = \case
  [] ->
    Left "Can't parse nothing as type"
  (Right _ : _) ->
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
  pullSimilar _ [] =
    return []
  pullSimilar _ _ =
    Left "Can't mix infix type operators with same precedence or over apply types"
  goOp OpComma xs = List.foldl1 PairT xs
  goOp OpFunctionArrow xs = List.foldr1 TypeArrow xs


one :: Parser s m => (Type Var -> x) -> m x
one x = x <$> (snd <$> pTypeSimple)

two :: Parser s m => (Type Var -> Type Var -> x) -> m x
two x = one x <*> (snd <$> pTypeSimple)

three :: Parser s m => (Type Var -> Type Var -> Type Var -> x) -> m x
three x = two x <*> (snd <$> pTypeSimple)
