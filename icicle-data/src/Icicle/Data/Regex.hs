{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Data.Regex (
    Regex (..)
  , Transition (..)

  , zero
  , epsilon
  , add
  , times
  , plus
  , star
  , once
  , dot
  , range

  , match
  , printC

  , printCWithTestHeaders
) where

import           Data.Bits ((.|.), (.&.))
import qualified Data.Bits as Bits
import qualified Data.Text as Text

import           GHC.Generics (Generic)

import           Icicle.Common.NanEq
import           Icicle.Internal.Pretty (Doc, vsep, indent, pretty)
import qualified Icicle.Internal.Pretty as Pretty
import           P

data Transition =
  Transition
    { transitionMatchByte :: Match
    , transitionStart     :: Int
    , transitionEndState  :: Integer
    } deriving (Eq, Ord, Show, Generic, NanEq)

data Regex =
  Regex
    { regexNumberOfStates         :: Int
    , regexStartingStates         :: Integer
    , regexTransitionFunction     :: [Transition]
    , regexAcceptingStates        :: Integer
    } deriving (Eq, Ord, Show, Generic, NanEq)


data Match
  = MatchChar Char
  | MatchRange Char Char
  | MatchAny
  deriving (Eq, Ord, Show, Generic, NanEq)


instance NFData Transition
instance NFData Regex
instance NFData Match


shiftTransition :: Int -> Transition -> Transition
shiftTransition j (Transition b i s) =
  Transition b (i + j) (Bits.unsafeShiftL s j)


zero :: Regex
zero =
  Regex 0 0 [] 0


epsilon :: Regex
epsilon =
  Regex 1 1 [] 1


add :: Regex -> Regex -> Regex
add (Regex nL asL fL bsL) (Regex nR asR fR bsR) =
    Regex n as f bs
  where
    n  = nL + nR
    as = Bits.unsafeShiftL asR nL .|. asL
    f  = fL <> fmap (shiftTransition nL) fR
    bs = Bits.unsafeShiftL bsR nL .|. bsL


times :: Regex -> Regex -> Regex
times (Regex nL asL fL bsL) (Regex nR asR fR bsR) =
    Regex n as f bs
  where
    n = nL + nR
    asR' = Bits.unsafeShiftL asR nL
    as =
      if asL .&. bsL == 0 then
        asL else
      asL .|. asR'

    remapLeft =
      flip fmap fL $ \(Transition b from to) ->
        let
          to1 =
            if to .&. bsL == 0 then
              to
            else
              to .|. asR'
        in
          Transition b from to1

    f = remapLeft <> fmap (shiftTransition nL) fR

    bs = Bits.unsafeShiftL bsR nL


star :: Regex -> Regex
star (Regex n as f bs) = Regex n as f' as
  where
    f' =
      flip fmap f $ \(Transition b from to) ->
        if to .&. bs == 0 then
          Transition b from to
        else
          Transition b from (to .|. as)


plus :: Regex -> Regex
plus (Regex n as f bs) = Regex n as f' bs
  where
    f' =
      flip fmap f $ \(Transition b from to) ->
        if to .&. bs == 0 then
          Transition b from to
        else
          Transition b from (to .|. as)


range :: Char -> Char -> Regex
range st fe = Regex 2 1 f 2
  where
    f = [Transition (MatchRange st fe) 0 2]


once :: Char -> Regex
once c = Regex 2 1 f 2
  where
    f = [Transition (MatchChar c) 0 2]


dot :: Regex
dot = Regex 2 1 f 2
  where
    f = [Transition MatchAny 0 2]


match :: Regex -> Text -> Bool
match (Regex _ as f bs) cs =
    bs .&. Text.foldl' step as cs /= 0
  where
    step s0 c =
      let
        check (Transition (MatchChar m) start _) =
          Bits.testBit s0 start && c == m

        check (Transition (MatchRange s fe) start _) =
          Bits.testBit s0 start && c >= s && c <= fe

        check (Transition MatchAny start _) =
          Bits.testBit s0 start

        pertinent =
          filter check f
      in
        foldl' (.|.) 0 (fmap transitionEndState pertinent)


printC :: Doc -> Regex -> Doc
printC name (Regex numStates starting transitions acceptance) =
  vsep [
      "ibool_t iregex_" <> name <> "(const istring_t str) {"
    , indent 2 $ vsep [
        "const char *s = (const istring_t) str;"
      , "uint64_t one = 1;"
      , initialiseVars
      , "while ('\\0' != *s) {"
      , indent 2 $ vsep [
          initialiseNext
        , vsep (fmap goIf transitions)
        , realiseUpdates
        ]
      , "s++;"
      , "}"
      , "if (" <> accept <> ") {"
      , "  return itrue;"
      , " } else {"
      , "  return ifalse;"
      , "}"
      ]
    , "}"
    ]

  where
    clip v i =
      (v `Bits.unsafeShiftR` (i * 64)) `mod` (2 ^ (64 :: Int))

    requiredVars =
      numStates `div` 64 + 1

    sepX s = s . flip fmap [0..requiredVars - 1]

    initialiseVars = sepX vsep $ \n ->
      "uint64_t current" <> pretty n <> " = " <> pretty (clip starting n) <> ";"

    initialiseNext = sepX vsep $ \n ->
      "uint64_t next" <> pretty n <> " = 0;"

    realiseUpdates = sepX vsep $ \n ->
      "current" <> pretty n <> " = " <> "next" <> pretty n <> ";"

    accept = sepX (Pretty.hcat . Pretty.punctuate " || ") $ \n ->
      "current" <> pretty n <> " & " <> pretty (clip acceptance n) <> "U"

    goIf (Transition MatchAny from to) =
      let
        (n, fromN) =
          from `divMod` 64
      in
        vsep [
          "if (current" <> pretty n <> " & (one << " <> pretty fromN <> ")) {"
        , indent 2 $ sepX vsep $ \m ->
            "next" <> pretty m <> " |= " <> pretty (clip to m) <> "U;"
        , "}"
        ]
    goIf (Transition (MatchRange s fe) from to) =
      let
        (n, fromN) =
          from `divMod` 64
      in
        vsep [
          "if ((current" <> pretty n <> " & (one << " <> pretty fromN <> ")) && *s >= '" <> pretty s <> "' && *s <= '" <> pretty fe <> "') {"
        , indent 2 $ sepX vsep $ \m ->
            "next" <> pretty m <> " |= " <> pretty (clip to m) <> "U;"
        , "}"
        ]
    goIf (Transition (MatchChar c) from to) =
      let
        (n, fromN) =
          from `divMod` 64
      in
        vsep [
          "if ((current" <> pretty n <> " & (one << " <> pretty fromN <> ")) && *s == '" <> pretty c <> "') {"
        , indent 2 $ sepX vsep $ \m ->
            "next" <> pretty m <> " |= " <> pretty (clip to m) <> "U;"
        , "}"
        ]

printCWithTestHeaders :: Doc -> Regex -> Doc
printCWithTestHeaders name r =
  vsep [
    "#include <stdint.h>"
  , "#include <stdlib.h>"
  , "typedef uint64_t ibool_t;"
  , "typedef const char *istring_t;"
  , "static const ibool_t ifalse = 0;"
  , "static const ibool_t itrue  = 1;"
  ] <> "\n" <> printC name r
