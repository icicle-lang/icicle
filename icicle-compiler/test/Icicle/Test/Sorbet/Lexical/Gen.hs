{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Sorbet.Lexical.Gen (
    jToken
  , jInteger
  , jRational
  , jString
  , jDate
  , jYearMonthDay
  , jVarId
  , jConId
  , jVarIdHead
  , jConIdHead
  , jIdTail
  , jVarOp
  , jConOp
  , jVarOpHead
  , jConOpHead
  , jOpTail
  , jHeadTail
  , notReserved
  , reserved
  ) where

import           Data.Scientific (Scientific, scientific)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Thyme (Day, YearMonthDay(..), gregorianValid)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import qualified Hedgehog.Range as Range

import           Icicle.Sorbet.Lexical.Syntax

import           P

import           Test.QuickCheck.Instances ()


jToken :: Gen Token
jToken =
  Gen.choice [
      pure Tok_LParen
    , pure Tok_RParen
    , pure Tok_LBrace
    , pure Tok_RBrace
    , pure Tok_LBracket
    , pure Tok_RBracket
    , pure Tok_Semi
    , pure Tok_Comma
    , pure Tok_Backtick
    , pure Tok_LArrowDash
    , pure Tok_RArrowDash
    , pure Tok_RArrowEquals
    , pure Tok_Equals
    , pure Tok_Colon
    , pure Tok_EqualsColon
    , pure Tok_At
    , pure Tok_Of
    , pure Tok_If
    , pure Tok_Then
    , pure Tok_Else
    , pure Tok_From
    , pure Tok_In
    , pure Tok_Let
    , pure Tok_Windowed
    , pure Tok_Group
    , pure Tok_Distinct
    , pure Tok_Filter
    , pure Tok_Latest
    , pure Tok_Between
    , pure Tok_And
    , pure Tok_Days
    , pure Tok_Months
    , pure Tok_Weeks
    , Tok_PrjId <$> jVarId
    , Tok_VarId <$> jVarId
    , Tok_ConId <$> jConId
    , Tok_VarOp <$> jVarOp
    , Tok_ConOp <$> jConOp
    , Tok_Integer <$> jInteger
    , Tok_Rational <$> jRational
    , Tok_String <$> jString
    , Tok_Date <$> jDate
    ]

jInteger :: Gen Integer
jInteger =
  abs <$> Gen.arbitrary

jRational :: Gen Scientific
jRational =
  scientific
    <$> maxSized 99999999999999999
    <*> Gen.arbitrary

maxSized :: Double -> Gen Integer
maxSized x_max =
  Gen.sized $ \n ->
    let
      pct = fromIntegral n / 100.0
    in
      Gen.integral (Range.linear 0 (round . exp $ log x_max * pct))

jString :: Gen Text
jString =
  Gen.text
    (Range.linear 0 100)
    Gen.unicode

jDate :: Gen Day
jDate =
  Gen.just $ fmap gregorianValid jYearMonthDay

jYearMonthDay :: Gen YearMonthDay
jYearMonthDay =
  YearMonthDay
    <$> Gen.integral (Range.constant 0 9999)
    <*> Gen.integral (Range.constant 1 12)
    <*> Gen.integral (Range.constant 1 31)

jVarId :: Gen Text
jVarId =
  jHeadTail jVarIdHead jIdTail

jConId :: Gen Text
jConId =
  jHeadTail jConIdHead jIdTail

jVarIdHead :: Gen Char
jVarIdHead =
  Gen.frequency [
      (26, Gen.lower)
    , (1, pure '_')
    ]

jConIdHead :: Gen Char
jConIdHead =
  Gen.upper

jIdTail :: Gen Char
jIdTail =
  Gen.frequency [
      (26, Gen.lower)
    , (26, Gen.upper)
    , (10, Gen.digit)
    , (1, pure '_')
    , (1, pure '\'')
    ]

jVarOp :: Gen Text
jVarOp =
  jHeadTail jVarOpHead jOpTail

jConOp :: Gen Text
jConOp =
  jHeadTail jConOpHead jOpTail

jVarOpHead :: Gen Char
jVarOpHead =
  Gen.element ("!#$%&*+./<=>?@\\^-~|" :: [Char])

jConOpHead :: Gen Char
jConOpHead =
  pure ':'

jOpTail :: Gen Char
jOpTail =
  Gen.element (":!#$%&*+./<=>?@\\^-~|" :: [Char])

jHeadTail :: Gen Char -> Gen Char -> Gen Text
jHeadTail jHead jTail =
  notReserved . fmap T.pack $
    (:) <$> jHead <*> Gen.list (Range.linear 0 10) jTail

notReserved :: Gen Text -> Gen Text
notReserved j =
   j >>= \txt -> do
    guard $
      not (Set.member txt reserved) &&
      not ("--" `T.isPrefixOf` txt) -- operators can't start with comment
    pure txt

reserved :: Set Text
reserved =
  Set.fromList [
      "<-"
    , "->"
    , "=>"
    , "="
    , ":"
    , "=:"
    , "@"
    , "_"
    , "of"
    , "if"
    , "then"
    , "else"
    , "from"
    , "in"
    , "~>"
    , "let"
    , "windowed"
    , "group"
    , "distinct"
    , "filter"
    , "latest"
    , "between"
    , "and"
    , "days"
    , "months"
    , "weeks"
    ]
