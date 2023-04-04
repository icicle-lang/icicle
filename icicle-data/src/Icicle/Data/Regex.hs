{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Data.Regex (
    Regex (..)
  , Transition (..)

  , Acceptor (..)

  , zero
  , epsilon
  , add
  , question
  , times
  , pow
  , plus
  , star
  , once
  , dot
  , bound
  , range
  , acceptors

  , match
  , printC

  , printCWithTestHeaders
) where

import           Data.Bits ((.|.), (.&.))
import qualified Data.Bits as Bits
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List as List
import           Data.Char (ord)
import qualified Data.Text as Text
import           Data.These (These (..))

import           GHC.Generics (Generic)

import           Icicle.Common.NanEq
import           Icicle.Internal.Pretty (Doc, vsep, indent, pretty)
import qualified Icicle.Internal.Pretty as Pretty
import           P
import qualified Prelude as Savage

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

data Acceptor
  = AcceptChar !Char
  | AcceptRange !Char !Char
  deriving (Eq, Ord, Show, Generic, NanEq)

data Match
  = MatchAcceptor Bool (NonEmpty Acceptor)
  | MatchAny
  deriving (Eq, Ord, Show, Generic, NanEq)


instance NFData Transition
instance NFData Regex
instance NFData Acceptor
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


question :: Regex -> Regex
question a = a `add` epsilon


times :: Regex -> Regex -> Regex
times (Regex nL asL fL bsL) (Regex nR asR fR bsR) =
    Regex n as f bs
  where
    n = nL + nR
    asR' = Bits.unsafeShiftL asR nL
    as =
      if asL .&. bsL == 0 then
        asL
      else
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


-- | Straight from the prelude.
--   I would love to make Regex a semiring and not have this, but
--   the prelude is just not specific enough.
pow :: Regex -> Int -> Regex
pow x0 y0 | y0 < 0    = Savage.error "Negative exponent"
          | y0 == 0   = epsilon
          | otherwise = f x0 y0
    where -- f : x0 ^ y0 = x ^ y
          f x y | even y    = f (x `times` x) (y `quot` 2)
                | y == 1    = x
                | otherwise = g (x `times` x) (y `quot` 2) x
          -- g : x0 ^ y0 = (x ^ y) * z
          g x y z | even y = g (x `times` x) (y `quot` 2) z
                  | y == 1 = x `times` z
                  | otherwise = g (x `times` x) (y `quot` 2) (x `times` z)


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
    f = [Transition (MatchAcceptor False (AcceptRange st fe:| [])) 0 2]

acceptors :: Bool -> NonEmpty Acceptor -> Regex
acceptors negation acs = Regex 2 1 f 2
  where
    f = [Transition (MatchAcceptor negation acs) 0 2]

once :: Char -> Regex
once c = Regex 2 1 f 2
  where
    f = [Transition (MatchAcceptor False (AcceptChar c :| [])) 0 2]

dot :: Regex
dot = Regex 2 1 f 2
  where
    f = [Transition MatchAny 0 2]


bound :: These Int Int -> Regex -> Regex
bound =
  go
    where
  go (This a) r
    = pow r a `times` star r
  go (These a b) r
    = pow r a `times` go (That (b - a)) r
  go (That b) r
    = foldl times epsilon (List.replicate b (question r))


match :: Regex -> Text -> Bool
match (Regex _ as f bs) cs =
    bs .&. Text.foldl' step as cs /= 0
  where
    checkAcceptor c (AcceptChar c') =
      c' == c
    checkAcceptor c (AcceptRange s fe) =
      c >= s && c <= fe
    negs neg =
      if neg then not else id

    step s0 c =
      let
        check (Transition (MatchAcceptor neg acs) start _) =
          Bits.testBit s0 start && negs neg (any (checkAcceptor c) acs)

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
        "// current code point value"
      , "int32_t cp;"
      , "istring_t next = utf8codepoint(str, &cp);"
      , "uint64_t one = 1;"
      , initialiseVars
      , "while (cp != 0) {"
      , indent 2 $ vsep [
          initialiseNext
        , vsep (fmap goIf transitions)
        , realiseUpdates
        , "next = utf8codepoint(next, &cp);"
        ]
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

    checkAcceptor (AcceptChar c) =
      "cp == " <> pretty (ord c)
    checkAcceptor (AcceptRange s fe) =
      "cp >= " <> pretty (ord s) <> " && cp <= " <> pretty (ord fe)

    checkAcceptors negator =
      let
        negOp =
          if negator then
            ("!" <>)
          else
            id
      in
        negOp . Pretty.parens . Pretty.hcat . Pretty.punctuate " || " . fmap checkAcceptor


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
    goIf (Transition (MatchAcceptor negator acs) from to) =
      let
        (n, fromN) =
          from `divMod` 64
      in
        vsep [
          "if ((current" <> pretty n <> " & (one << " <> pretty fromN <> ")) && " <> checkAcceptors negator acs <> ") {"
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
  , "static istring_t utf8codepoint(const istring_t str,"
  , "                  int32_t * out_codepoint) {"
  , "const char *s = (const char *)str;"
  , "if (0xf0 == (0xf8 & s[0])) {"
  , "  // 4 byte utf8 codepoint"
  , "  *out_codepoint = ((0x07 & s[0]) << 18) | ((0x3f & s[1]) << 12) |"
  , "                   ((0x3f & s[2]) << 6) | (0x3f & s[3]);"
  , "  s += 4;"
  , "} else if (0xe0 == (0xf0 & s[0])) {"
  , "  // 3 byte utf8 codepoint"
  , "  *out_codepoint ="
  , "      ((0x0f & s[0]) << 12) | ((0x3f & s[1]) << 6) | (0x3f & s[2]);"
  , "  s += 3;"
  , "} else if (0xc0 == (0xe0 & s[0])) {"
  , "  // 2 byte utf8 codepoint"
  , "  *out_codepoint = ((0x1f & s[0]) << 6) | (0x3f & s[1]);"
  , "  s += 2;"
  , "} else {"
  , "  // 1 byte utf8 codepoint otherwise"
  , "  *out_codepoint = s[0];"
  , "  s += 1;"
  , "}"
  , ""
  , "return s;"
  , "}"
  ] <> "\n" <> printC name r
