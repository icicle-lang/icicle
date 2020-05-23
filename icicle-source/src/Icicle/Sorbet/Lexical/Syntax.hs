{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sorbet.Lexical.Syntax (
    Token(..)
  , renderToken
  ) where

import           Data.Data (Data)
import           Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Lazy
import qualified Data.Text.Lazy.Builder.Int as Lazy
import qualified Data.Text.Lazy.Builder.Scientific as Lazy
import           Data.Thyme.Calendar (Day, showGregorian)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty
import           Icicle.Sorbet.Lexical.Escape

import           P

data Token =
  --
  -- Delimiters
  --
    Tok_LParen       -- ^  (
  | Tok_RParen       -- ^  )
  | Tok_LBrace       -- ^  {
  | Tok_RBrace       -- ^  }
  | Tok_LBracket     -- ^  [
  | Tok_RBracket     -- ^  ]
  | Tok_Semi         -- ^  ;
  | Tok_Comma        -- ^  ,
  | Tok_Backtick     -- ^  `

  --
  -- Reserved Operators
  --
  | Tok_LArrowDash   -- ^  <-
  | Tok_RArrowDash   -- ^  ->
  | Tok_RArrowEquals -- ^  =>
  | Tok_Equals       -- ^  =
  | Tok_Colon        -- ^  :
  | Tok_EqualsColon  -- ^  =:
  | Tok_At           -- ^  @
  | Tok_FlowsInto    -- ^  ~>

  --
  -- Reserved Identifiers
  --
  | Tok_Dictionary
  | Tok_Module
  | Tok_Import
  | Tok_Where
  | Tok_Input
  | Tok_Feature
  --
  | Tok_Wild
  | Tok_Case
  | Tok_Of
  | Tok_If
  | Tok_Then
  | Tok_Else
  | Tok_From
  | Tok_In
  --
  | Tok_Let
  | Tok_Fold
  | Tok_Fold1
  | Tok_Windowed
  | Tok_Group
  | Tok_Distinct
  | Tok_Filter
  | Tok_Latest
  --
  | Tok_Between
  | Tok_Before
  | Tok_After
  | Tok_And
  | Tok_Seconds
  | Tok_Days
  | Tok_Months
  | Tok_Weeks

  --
  -- Identifiers
  --
  | Tok_PrjId !Text -- ^ .<var-id>
  | Tok_VarId !Text
  | Tok_ConId !Text

  --
  -- Operators
  --
  | Tok_VarOp !Text
  | Tok_ConOp !Text

  --
  -- Literals
  --
  | Tok_Integer  !Integer
  | Tok_Rational !Scientific
  | Tok_String   !Text
  | Tok_Date     !Day
    deriving (Eq, Ord, Show, Generic, Data, Typeable)

-- | Render a token as its canonical lexeme.
renderToken :: Token -> Text
renderToken = \case
  Tok_LParen            -> "("
  Tok_RParen            -> ")"
  Tok_LBrace            -> "{"
  Tok_RBrace            -> "}"
  Tok_LBracket          -> "["
  Tok_RBracket          -> "]"
  Tok_Semi              -> ";"
  Tok_Comma             -> ","
  Tok_Backtick          -> "`"
  Tok_LArrowDash        -> "<-"
  Tok_RArrowDash        -> "->"
  Tok_RArrowEquals      -> "=>"
  Tok_Equals            -> "="
  Tok_Colon             -> ":"
  Tok_EqualsColon       -> "=:"
  Tok_At                -> "@"
  Tok_FlowsInto         -> "~>"
  Tok_Dictionary        -> "dictionary"
  Tok_Module            -> "module"
  Tok_Import            -> "import"
  Tok_Where             -> "where"
  Tok_Input             -> "input"
  Tok_Feature           -> "feature"
  Tok_Wild              -> "_"
  Tok_Case              -> "case"
  Tok_Of                -> "of"
  Tok_If                -> "if"
  Tok_Then              -> "then"
  Tok_Else              -> "else"
  Tok_From              -> "from"
  Tok_In                -> "in"
  Tok_Let               -> "let"
  Tok_Fold              -> "fold"
  Tok_Fold1             -> "fold1"
  Tok_Windowed          -> "windowed"
  Tok_Group             -> "group"
  Tok_Distinct          -> "distinct"
  Tok_Filter            -> "filter"
  Tok_Latest            -> "latest"
  Tok_Between           -> "between"
  Tok_Before            -> "before"
  Tok_After             -> "after"
  Tok_And               -> "and"
  Tok_Seconds           -> "seconds"
  Tok_Days              -> "days"
  Tok_Months            -> "months"
  Tok_Weeks             -> "weeks"
  Tok_PrjId    varId    -> "." <> varId
  Tok_VarId    varId    -> varId
  Tok_ConId    conId    -> conId
  Tok_VarOp    varOp    -> varOp
  Tok_ConOp    conOp    -> conOp
  Tok_Integer  integer  -> fromBuilder $ Lazy.decimal integer
  Tok_Rational rational -> fromBuilder $ Lazy.scientificBuilder rational
  Tok_String   string   -> quotedString string
  Tok_Date     day      -> T.pack $ showGregorian day

fromBuilder :: Lazy.Builder -> Text
fromBuilder =
  Lazy.toStrict . Lazy.toLazyText

quotedString :: Text -> Text
quotedString =
  T.pack . enquote . escapeChars . T.unpack

enquote :: [Char] -> [Char]
enquote str =
  "\"" <> str <> "\""

instance Pretty Token where
  pretty =
    pretty . renderToken
