{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Icicle.Sorbet.Position (
    Positioned(..)
  , Position(..)
  , PositionedStream(..)
  , Range(..)

  , renderPosition
  , toParsec
  , fromParsec
  , fromSourcePos
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import           Data.Data (Data)
import           Data.Typeable (Typeable)
import qualified Data.Text as Text

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty

import           P

import           System.IO (FilePath)

import           Text.Megaparsec (Stream(..), PosState (..))
import           Text.Megaparsec.Pos (SourcePos(..), mkPos, unPos)
import qualified Text.Parsec.Pos as Parsec

import           X.Text.Show (gshowsPrec)

data Positioned a =
  Positioned {
      posStart :: !Position
    , posEnd :: !Position
    , posTail :: !a
    } deriving (Eq, Ord, Generic, Data, Typeable, Functor)

data Position =
  Position {
      posFile :: !FilePath
    , posLine :: !Int
    , posColumn :: !Int
    } deriving (Eq, Ord, Generic, Data, Typeable)

data Range =
  Range {
      rangeStart :: !Position
    , rangeEnd :: !Position
  } deriving (Eq, Ord, Generic, Data, Typeable)

instance Show a => Show (Positioned a) where
  showsPrec =
    gshowsPrec

instance Show Position where
  showsPrec =
    gshowsPrec

instance Show Range where
  showsPrec =
    gshowsPrec

instance NFData Position

instance NFData Range

data PositionedStream a =
  PositionedStream {
    streamText :: Text
  , streamToks :: [Positioned a]
  } deriving (Eq, Show)


instance (Pretty a, Ord a) => Stream (PositionedStream a) where
  type Token (PositionedStream a) =
    Positioned a

  type Tokens (PositionedStream a) =
    [Positioned a]

  tokenToChunk Proxy =
    pure
  tokensToChunk Proxy =
    id
  chunkToTokens Proxy =
    id
  chunkLength Proxy =
    length
  chunkEmpty Proxy =
    null
  take1_ (PositionedStream _ [])
    = Nothing
  take1_ (PositionedStream str (t:ts))
    = Just ( t , PositionedStream str ts)

  takeN_ n (PositionedStream str s)
    | n <= 0    = Just ([], PositionedStream str s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in Just (x, PositionedStream str s')

  takeWhile_ f (PositionedStream str s) =
    let (x, s') = List.span f s
     in (x, PositionedStream str s')


  showTokens _ =
    List.intercalate " " . NonEmpty.toList . fmap (show . pretty . posTail)

  reachOffset o (PosState input offset sourcePos tabWidth _) =
    ( newSourcePos
    , fromMaybe "" thisLine
    , PosState
        { pstateInput = PositionedStream (streamText input) post
        , pstateOffset = max offset o
        , pstateSourcePos = newSourcePos
        , pstateTabWidth = tabWidth
        , pstateLinePrefix = ""
        }
    )
    where
      newSourcePos =
        case post of
          [] ->    sourcePos
          (x:_) -> toSourcePos $ posStart x

      post =
        drop (o - offset) (streamToks input)

      (!?) :: [a] -> Int -> Maybe a
      (!?) [] _ = Nothing
      (!?) (x:xs) n | n == 0 = return x
                    | n < 0 = Nothing
                    | otherwise = (!?) xs (n-1)

      thisLine = fmap Text.unpack $
        Text.lines (streamText input) !? (unPos (sourceLine newSourcePos) - 1)

toSourcePos :: Position -> SourcePos
toSourcePos = \case
  Position file srcLine srcCol ->
    SourcePos file
      (mkPos srcLine)
      (mkPos srcCol)


fromSourcePos :: SourcePos -> Position
fromSourcePos = \case
  SourcePos file srcLine col ->
    Position file
      (fromIntegral $ unPos srcLine)
      (fromIntegral $ unPos col)


toParsec :: Position -> Parsec.SourcePos
toParsec = \case
  Position file srcLine col ->
    Parsec.newPos file srcLine col



fromParsec ::  Parsec.SourcePos -> Position
fromParsec =
  Position <$> Parsec.sourceName <*> Parsec.sourceLine <*> Parsec.sourceColumn


instance Pretty Position where
  pretty =
    pretty . renderPosition

renderPosition :: Position -> Text
renderPosition sp =
  Text.pack (show . posLine $ sp) <> ":" <> Text.pack (show . posColumn $ sp)
    <> (if posFile sp == ""
        then ""
        else ":" <> Text.pack (posFile sp))

instance Pretty Range where
  pretty =
    pretty . rangeStart
