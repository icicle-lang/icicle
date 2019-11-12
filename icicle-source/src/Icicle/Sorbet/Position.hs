{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Icicle.Sorbet.Position (
    Positioned(..)
  , Position(..)
  ) where

import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import           Data.Data (Data)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

import           P

import           System.IO (FilePath)

import           Text.Megaparsec (Stream(..))
import           Text.Megaparsec.Pos (SourcePos(..), mkPos)

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

instance Show a => Show (Positioned a) where
  showsPrec =
    gshowsPrec

instance Show Position where
  showsPrec =
    gshowsPrec

instance (Ord a, Show a) => Stream [Positioned a] where
  type Token [Positioned a] =
    Positioned a

  type Tokens [Positioned a] =
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
  take1_ [] =
    Nothing
  take1_ (t:ts) =
    Just (t, ts)

  takeN_ n s
    | n <= 0
    = Just ([], s)
    | null s
    = Nothing
    | otherwise
    = Just (splitAt n s)

  takeWhile_ =
    List.span

  showTokens _ =
    show . fmap posTail
