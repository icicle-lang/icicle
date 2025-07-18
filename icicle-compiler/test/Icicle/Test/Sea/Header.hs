{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Sea.Header where

import           Hedgehog.Corpus
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Common.Type
import           Icicle.Data.Name
import           Icicle.Sea.Data
import           Icicle.Sea.Header
import           Icicle.Sea.Name

import           P

import qualified Prelude as Savage

import           System.IO (IO)

genFingerprint :: Gen Fingerprint
genFingerprint =
  Fingerprint
    <$> Gen.element muppets

genInputId :: Gen InputId
genInputId =
  Gen.just . fmap parseInputId $
    (\ns n -> ns <> ":" <> n)
      <$> Gen.element colours
      <*> Gen.element simpsons

genOutputId :: Gen OutputId
genOutputId =
  Gen.just . fmap parseOutputId $
    (\ns n -> ns <> ":" <> n)
      <$> Gen.element weather
      <*> Gen.element cooking

genStructField :: Gen StructField
genStructField =
  StructField
    <$> Gen.element viruses

genValType :: Gen ValType
genValType =
  Gen.recursive Gen.choice [
      pure BoolT
    , pure TimeT
    , pure DoubleT
    , pure IntT
    , pure StringT
    , pure UnitT
    , pure ErrorT
    ] [
      ArrayT <$> genValType
    , MapT <$> genValType <*> genValType
    , OptionT <$> genValType
    , PairT <$> genValType <*> genValType
    , SumT <$> genValType <*> genValType
    , StructT . StructType . Map.fromList
        <$> Gen.list (Range.linear 1 5) ((,) <$> genStructField <*> genValType)
    , BufT <$> Gen.integral (Range.linear 1 100) <*> genValType
    ]

genMeltedType :: Gen MeltedType
genMeltedType = do
  vtype <- genValType
  pure $
    MeltedType vtype (meltType vtype)

genClusterId :: Gen ClusterId
genClusterId =
  ClusterId
    <$> Gen.integral (Range.linear 1 10000)

genKernelIndex :: Gen KernelIndex
genKernelIndex =
  KernelIndex
    <$> Gen.integral (Range.linear 1 10000)

genKernelId :: Gen KernelId
genKernelId =
  KernelId
    <$> genClusterId
    <*> genKernelIndex

genSeaName :: Gen SeaName
genSeaName =
  mangle <$>
    Gen.element (southpark :: [Text])

genKernel :: Gen (Kernel ())
genKernel =
  Kernel
    <$> genKernelId
    <*> Gen.list (Range.linear 1 5) ((,) <$> genOutputId <*> genMeltedType)
    <*> pure ()

genCluster :: Gen (Cluster () ())
genCluster =
  Cluster
    <$> genClusterId
    <*> genInputId
    <*> genValType
    <*> Gen.list (Range.linear 1 5) ((,) <$> genSeaName <*> genValType)
    <*> genSeaName
    <*> (NonEmpty.fromList <$> Gen.list (Range.linear 1 10) genKernel)
    <*> pure ()

genHeader :: Gen Header
genHeader =
  Header
    <$> genFingerprint
    <*> Gen.list (Range.linear 1 5) genCluster

snippet :: Text
snippet =
  Text.unlines [
      "int main (int argc, char **argv) {"
    , "    return 0;"
    , "}"
    ]

renderHeaderX :: Header -> Text
renderHeaderX x =
  renderHeader x <> snippet

parseHeaderX :: Text -> Either HeaderDecodeError Header
parseHeaderX x = do
  (header, code) <- parseHeader x
  if code /= snippet then
    Savage.error $
      show code <>
      "\n=/=\n" <>
      show snippet
  else
    pure header

prop_header_roundtrip :: Property
prop_header_roundtrip =
  property $ do
    tokens <- forAll genHeader
    tripping tokens renderHeaderX parseHeaderX


tests :: IO Bool
tests =
  checkParallel $$(discover)
