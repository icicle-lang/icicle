{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Runtime.Serial.Psv.Data (
    SerialPsvDataError(..)
  , renderSerialPsvDataError

  , encodeSnapshotOutput
  , encodeChordOutput
  ) where

import qualified Anemone.Pretty as Anemone

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Storable as Storable

import           Icicle.Data.Name
import           Icicle.Internal.Aeson
import           Icicle.Runtime.Data
import qualified Icicle.Runtime.Data.Logical as Logical
import qualified Icicle.Runtime.Data.Schema as Schema
import qualified Icicle.Runtime.Data.Striped as Striped

import           P

import           Text.Show.Pretty (ppShow)
import           Text.Printf (printf)

import qualified X.Data.Vector as Boxed
import qualified X.Data.Vector.Cons as Cons

import qualified Zebra.X.Vector.Segment as Segment


data SerialPsvDataError =
    SerialPsvDataUnsupportedSchema !Schema
  | SerialPsvDataUnsupportedValue !Value
  | SerialPsvDataSegmentError !Schema !Segment.SegmentError
  | SerialPsvDataStripedError !Striped.StripedError
  | SerialPsvDataOutputError !OutputId !SerialPsvDataError
  | SerialPsvDataMissingValue !Error64
    deriving (Eq, Show)

renderSerialPsvDataError :: SerialPsvDataError -> Text
renderSerialPsvDataError = \case
  SerialPsvDataUnsupportedSchema x ->
    "Unsupported PSV output: " <> Text.pack (ppShow x)
  SerialPsvDataUnsupportedValue x ->
    "Unsupported PSV output: " <> Text.pack (ppShow x)
  SerialPsvDataSegmentError s x ->
    "Error encoding PSV, invalid segment descriptor for " <> Text.pack (ppShow s) <> ": " <> Segment.renderSegmentError x
  SerialPsvDataStripedError x ->
    "Error encoding PSV: " <> Striped.renderStripedError x
  SerialPsvDataOutputError oid x ->
    "Error encoding <" <> renderOutputId oid <> "> output: " <> renderSerialPsvDataError x
  SerialPsvDataMissingValue x ->
    "Internal error, found missing value: " <> Text.pack (ppShow x)

encodeBool :: Bool64 -> Builder
encodeBool = \case
  False64 ->
    "false"
  _ ->
    "true"
{-# INLINE encodeBool #-}

encodeInt :: Int64 -> Builder
encodeInt =
  Builder.int64Dec
{-# INLINE encodeInt #-}

encodeDouble :: Double -> Builder
encodeDouble =
  Builder.byteString . Anemone.renderDouble
{-# INLINE encodeDouble #-}

encodeDateString :: Time64 -> String
encodeDateString x =
  let
    UnpackedTime64 y m d _ =
      unpackTime x
  in
    printf "%04d-%02d-%02d" y m d

encodeDate :: Time64 -> Builder
encodeDate =
  Builder.byteString . Char8.pack . encodeDateString

encodeMissing :: Eq a => Builder -> a -> a -> Builder -> Builder
encodeMissing missing tag_ok tag value =
  if tag_ok == tag then
    value
  else
    missing
{-# INLINE encodeMissing #-}

ppValue :: Schema -> Value -> Either SerialPsvDataError Aeson.Value
ppValue schema value =
  case value of
    Logical.Unit
      | Schema.Unit <- schema
      -> pure $ Aeson.object []
    Logical.Bool False64
      | Schema.Bool <- schema
      -> pure $ Aeson.toJSON False
    Logical.Bool _
      | Schema.Bool <- schema
      -> pure $ Aeson.toJSON True
    Logical.Int x
      | Schema.Int <- schema
      -> pure $ Aeson.toJSON x
    Logical.Double x
      | Schema.Double <- schema
      -> pure $ Aeson.toJSON x
    Logical.Time x
      | Schema.Time <- schema
      -> pure . Aeson.toJSON $ encodeDateString x
    Logical.Left _ ->
      Left $
        SerialPsvDataUnsupportedValue value
    Logical.Right _ ->
      Left $
        SerialPsvDataUnsupportedValue value
    Logical.None
      | Schema.Option _ <- schema
      -> pure Aeson.Null
    Logical.Some x
      | Schema.Option s <- schema
      -> ppValue s x
    Logical.Error x ->
      Left $
        SerialPsvDataMissingValue x
    Logical.Success x
      | Schema.Result s <- schema
      -> ppValue s x
    Logical.Pair x0 y0
      | Schema.Pair sx sy <- schema
      -> do x <- ppValue sx x0
            y <- ppValue sy y0
            pure $ Aeson.toJSON [x, y]
    Logical.Struct xs0
      | Schema.Struct sxs0 <- schema
      , Cons.length xs0 == Cons.length sxs0
      -> let takeField (Field name s) x = (name,) <$> ppValue s x
         in  Aeson.object . Cons.toList <$> Cons.zipWithM takeField sxs0 xs0

    Logical.String x
      | Schema.String <- schema
      -> pure . Aeson.toJSON $ Text.decodeUtf8 x
    Logical.Array xs
      | Schema.Array s <- schema
      -> Aeson.toJSON <$> traverse (ppValue s) xs
    Logical.Map kvs
      | Schema.Map sk sv <- schema
      -> Aeson.toJSON <$> traverse (bitraverse (ppValue sk) (ppValue sv)) (Map.toList kvs)

    _ ->
      Left $
        SerialPsvDataUnsupportedValue value

encodeValues :: Aeson.Value -> Builder
encodeValues =
  Builder.lazyByteString . encodeCompactJson' []

encodeColumn :: Column -> Either SerialPsvDataError (Boxed.Vector Builder)
encodeColumn column =
  case column of
    Striped.Unit n ->
      pure $
        Boxed.replicate n "{}"

    Striped.Bool xs ->
      pure $
        Boxed.map encodeBool $ Storable.convert xs

    Striped.Int xs ->
      pure $
        Boxed.map encodeInt $ Storable.convert xs

    Striped.Double xs ->
      pure $
        Boxed.map encodeDouble $ Storable.convert xs

    Striped.Time xs ->
      pure $
        Boxed.map encodeDate $ Storable.convert xs

    Striped.Sum _ _ _ -> do
      Left $
        SerialPsvDataUnsupportedSchema (Striped.schema column)

    Striped.Option tags x -> do
      values <- encodeColumn x
      pure $
        Boxed.zipWith (encodeMissing "NA" True64) (Storable.convert tags) values

    Striped.Result tags x -> do
      values <- encodeColumn x
      pure $
        Boxed.zipWith (encodeMissing "NA" NotAnError64) (Storable.convert tags) values

    Striped.String ns bs ->
      bimap
        (SerialPsvDataSegmentError Schema.String)
        (fmap encodeQuotedString) $
        Segment.reify ns bs

    Striped.Pair {} ->
      encodeAsJson column

    Striped.Struct {} ->
      encodeAsJson column

    Striped.Array {} ->
      encodeAsJson column

    Striped.Map {} ->
      encodeAsJson column

-- Encode strings which contain delimiter characters
-- as quoted json, otherwise, just leave them alone.
encodeQuotedString :: ByteString.ByteString -> Builder
encodeQuotedString fld =
  if ByteString.any (\b -> b == dquote || b == pipe || b == nl) fld then
    Aeson.fromEncoding . Aeson.toEncoding . Text.decodeUtf8 $ fld
  else
    Builder.byteString fld

  where
    dquote =
      34
    pipe =
      124
    nl =
      10

encodeAsJson :: Column -> Either SerialPsvDataError (Boxed.Vector Builder)
encodeAsJson column = do
  values <- first SerialPsvDataStripedError $ Striped.toLogical column
  traverse (fmap encodeValues . (ppValue $ Striped.schema column)) values


encodeSnapshotKey :: SnapshotKey -> Builder
encodeSnapshotKey =
  Builder.byteString . unEntityId . entityId . snapshotEntity

encodeChordKey :: ChordKey -> Builder
encodeChordKey x =
  Builder.byteString (unEntityId . entityId $ chordEntity x) <> "|" <>
  Builder.byteString (chordLabel x)

encodeColumns :: Boxed.Vector (Boxed.Vector Builder) -> Boxed.Vector Builder
encodeColumns =
  Boxed.map (mconcat . List.intersperse "|" . Boxed.toList) .
  Boxed.transpose

encodeRows :: Boxed.Vector Builder -> Builder
encodeRows =
  mconcat . Boxed.toList . fmap (<> "\n")

encodeOutput :: (key -> Builder) -> Output key -> Either SerialPsvDataError Builder
encodeOutput encodeKey output = do
  let
    key =
      fmap encodeKey (outputKey output)

    encode (k, v) =
      first (SerialPsvDataOutputError k) (encodeColumn v)

  columns <- traverse encode . Map.toList $ outputColumns output

  pure .
    encodeRows . encodeColumns . Boxed.fromList $ key : columns

encodeSnapshotOutput :: Output SnapshotKey -> Either SerialPsvDataError Builder
encodeSnapshotOutput output =
  encodeOutput encodeSnapshotKey output

encodeChordOutput :: Output ChordKey -> Either SerialPsvDataError Builder
encodeChordOutput output =
  encodeOutput encodeChordKey output
