{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Test.Runtime.Serial.Psv.Schema where

import           Hedgehog.Corpus (muppets, boats, viruses)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Icicle.Runtime.Serial.Psv.Schema

import           P

import           System.IO (IO)

genPrimitive :: Gen PsvPrimitive
genPrimitive =
  Gen.element [
      PsvBoolean
    , PsvInt
    , PsvDouble
    , PsvString
    , PsvDate
    ]

genStructField :: Gen PsvStructField
genStructField =
  PsvStructField
    <$> Gen.element muppets
    <*> genEncoding

genEncoding :: Gen PsvEncoding
genEncoding =
  Gen.recursive Gen.choice [
      PsvPrimitive <$> genPrimitive
    ] [
      PsvStruct <$> Gen.list (Range.linear 0 20) genStructField
    , PsvList <$> genEncoding
    , PsvPair <$> genEncoding <*> genEncoding
    ]

genColumn :: Gen PsvColumn
genColumn =
  PsvColumn
    <$> Gen.element boats
    <*> genEncoding

genMissingValue :: Gen PsvMissingValue
genMissingValue =
  PsvMissingValue
    <$> Gen.element viruses

genSchema :: Gen PsvSchema
genSchema =
  PsvSchema
    <$> genMissingValue
    <*> Gen.list (Range.linear 0 20) genColumn

prop_psv_pretty_schema_roundtrip :: Property
prop_psv_pretty_schema_roundtrip =
  property $ do
    x <- forAll genSchema
    tripping x renderPrettyPsvSchema parsePsvSchema

prop_psv_compact_schema_roundtrip :: Property
prop_psv_compact_schema_roundtrip =
  property $ do
    x <- forAll genSchema
    tripping x renderCompactPsvSchema parsePsvSchema


tests :: IO Bool
tests =
  checkParallel $$(discover)
