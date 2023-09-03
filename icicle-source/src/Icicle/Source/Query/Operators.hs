{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Operators (
    Op            (..)
  , ArithUnary    (..)
  , ArithBinary   (..)
  , ArithDouble   (..)
  , Relation      (..)
  , LogicalBinary (..)
  , LogicalUnary  (..)
  , TimeBinary    (..)
  , Fixity        (..)
  , Infixity      (..)
  , Assoc         (..)
  , OpsOfSymbol   (..)
  , fixity
  , symbol
  , precedencePrefix
  , precedenceApplication
  , precedenceAlwaysParens
  , precedenceNeverParens
  ) where

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty

import           P


data Op
 = ArithUnary    ArithUnary
 | ArithBinary   ArithBinary
 | ArithDouble   ArithDouble
 | Relation      Relation
 | LogicalUnary  LogicalUnary
 | LogicalBinary LogicalBinary
 | TimeBinary    TimeBinary
 | Dollar
 deriving (Show, Eq, Ord, Generic)

instance NFData Op

data ArithUnary
 = Negate
 deriving (Show, Eq, Ord, Generic)

instance NFData ArithUnary

data ArithBinary
 = Mul
 | Add
 | Sub
 deriving (Show, Eq, Ord, Generic)

instance NFData ArithBinary

data ArithDouble
 = Div
 | Pow
 deriving (Show, Eq, Ord, Generic)

instance NFData ArithDouble

data Relation
 = Lt
 | Le
 | Gt
 | Ge
 | Eq
 | Ne
 deriving (Show, Eq, Ord, Generic)

instance NFData Relation

data LogicalUnary
 = Not
 deriving (Show, Eq, Ord, Generic)

instance NFData LogicalUnary

data LogicalBinary
 = And
 | Or
 deriving (Show, Eq, Ord, Generic)

instance NFData LogicalBinary

data TimeBinary
 = DaysBefore
 | DaysAfter
 | WeeksBefore
 | WeeksAfter
 | MonthsBefore
 | MonthsAfter
 deriving (Show, Eq, Ord, Generic)

instance NFData TimeBinary

data Fixity
 = FInfix  Infixity
 | FPrefix
 deriving (Show, Eq, Ord, Generic)

instance NFData Fixity

data Infixity
 = Infix Assoc Int
 deriving (Show, Eq, Ord, Generic)

instance NFData Infixity

data Assoc
 = AssocLeft | AssocRight
 deriving (Show, Eq, Ord, Generic)

instance NFData Assoc

fixity :: Op -> Fixity
fixity o
 = case o of
    ArithUnary _
     -> FPrefix

    ArithBinary Mul
     -> FInfix $ Infix AssocLeft 7
    ArithBinary Add
     -> FInfix $ Infix AssocLeft 6
    ArithBinary Sub
     -> FInfix $ Infix AssocLeft 6

    ArithDouble Div
     -> FInfix $ Infix AssocLeft 7
    ArithDouble Pow
     -> FInfix $ Infix AssocRight 8

    Relation _
     -> FInfix $ Infix AssocLeft 4

    LogicalUnary Not
     -> FPrefix
    LogicalBinary And
     -> FInfix $ Infix AssocLeft 3
    LogicalBinary Or
     -> FInfix $ Infix AssocLeft 2

    TimeBinary _
     -> FInfix $ Infix AssocLeft 6

    Dollar
        -> FInfix $ Infix AssocRight 0


data OpsOfSymbol
 = OpsOfSymbol
 { opInfix  :: Maybe Op
 , opPrefix :: Maybe Op }
 deriving (Show, Eq, Ord, Generic)


symbol :: Text -> OpsOfSymbol
symbol s
 = case s of
    "/" -> inf (ArithDouble Div)
    "*" -> inf (ArithBinary Mul)
    "+" -> inf (ArithBinary Add)
    "^" -> inf (ArithDouble Pow)
    "-" -> OpsOfSymbol (Just $ ArithBinary Sub) (Just $ ArithUnary Negate)

    ">" -> inf $ Relation Gt
    ">="-> inf $ Relation Ge
    "<" -> inf $ Relation Lt
    "<="-> inf $ Relation Le
    "=="-> inf $ Relation Eq
    "/="-> inf $ Relation Ne

    "!" -> pre $ LogicalUnary  Not
    "&&"-> inf $ LogicalBinary  And
    "||"-> inf $ LogicalBinary  Or

    "days before"   -> inf $ TimeBinary DaysBefore
    "days after"    -> inf $ TimeBinary DaysAfter
    "weeks before"  -> inf $ TimeBinary WeeksBefore
    "weeks after"   -> inf $ TimeBinary WeeksAfter
    "months before" -> inf $ TimeBinary MonthsBefore
    "months after"  -> inf $ TimeBinary MonthsAfter

    "$" -> inf Dollar

    _   -> OpsOfSymbol  Nothing    Nothing
 where
  inf o = OpsOfSymbol (Just o) Nothing
  pre o = OpsOfSymbol Nothing (Just o)

-- | Prefix operators are baked in to the parser, but these are used for pretty printing.
precedencePrefix :: (Int,Assoc)
precedencePrefix = (9, AssocLeft)

-- | Applications are baked in to the parser, but these are used for pretty printing.
precedenceApplication :: (Int,Assoc)
precedenceApplication = (10, AssocLeft)

-- | Wrap this in parentheses no matter what.
precedenceAlwaysParens :: (Int,Assoc)
precedenceAlwaysParens = (-1, AssocLeft)

-- | Never wrap this in parentheses: variable names, primitives etc
precedenceNeverParens :: (Int,Assoc)
precedenceNeverParens = (11, AssocLeft)


instance Pretty Op where
 pretty (ArithUnary Negate)     = "-"
 pretty (ArithBinary Mul)       = "*"
 pretty (ArithBinary Add)       = "+"
 pretty (ArithBinary Sub)       = "-"
 pretty (ArithDouble Pow)       = "^"
 pretty (ArithDouble Div)       = "/"

 pretty (Relation Lt)           = "<"
 pretty (Relation Le)           = "<="
 pretty (Relation Gt)           = ">"
 pretty (Relation Ge)           = ">="
 pretty (Relation Eq)           = "=="
 pretty (Relation Ne)           = "/="

 pretty (LogicalUnary Not)      = "!"
 pretty (LogicalBinary And)     = "&&"
 pretty (LogicalBinary Or)      = "||"

 pretty (TimeBinary DaysAfter)    = "days after"
 pretty (TimeBinary DaysBefore)   = "days before"
 pretty (TimeBinary WeeksAfter)   = "weeks after"
 pretty (TimeBinary WeeksBefore)  = "weeks before"
 pretty (TimeBinary MonthsAfter)  = "months after"
 pretty (TimeBinary MonthsBefore) = "months before"

 pretty Dollar                  = "$"

