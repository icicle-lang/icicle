{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Icicle.Source.Lexer.Token (
    Operator (..)
  , Variable (..)
  ) where

import Icicle.Internal.Pretty

import                  P
import                  Data.String
import qualified        Data.Text as T
import                  Data.Hashable (Hashable)

import                  GHC.Generics


newtype Operator
 = Operator Text
 deriving (Eq, Ord, Show)

newtype Variable
 = Variable Text
 deriving (Eq, Ord, Show, Generic)

instance Hashable Variable
instance NFData Variable

instance Pretty Variable where
 pretty (Variable v)
   = text
   $ T.unpack v

instance IsString Variable where
 fromString s = Variable $ T.pack s
