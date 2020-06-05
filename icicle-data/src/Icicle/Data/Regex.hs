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

  , match
  , printC

  , printCWithTestHeaders
) where

import           Data.Bits ((.|.), (.&.))
import qualified Data.Bits as Bits
import           Data.Word (Word64, Word8)

import           Icicle.Internal.Pretty (Doc, vsep, indent, pretty)
import qualified Icicle.Internal.Pretty as Pretty
import           P

data Transition =
  Transition
    { transitionMatchByte :: Maybe (Char)
    , transitionStart :: Int
    , transitionEndState :: Word64
    } deriving (Eq, Show)

data Regex =
  Regex
    { regexNumberOfStates         :: Int
    , regexStartingStates         :: Word64
    , regexTransitionFunction     :: [Transition]
    , regexAcceptingStates        :: Word64
    } deriving (Eq, Show)


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


once :: Char -> Regex
once c = Regex 2 1 f 2
  where
    f = [Transition (Just c) 0 2]


dot :: Regex
dot = Regex 2 1 f 2
  where
    f = [Transition Nothing 0 2]


match :: Regex -> [Char] -> Bool
match (Regex _ as f bs) cs =
    bs .&. foldl' step as cs /= 0
  where
    step s0 c =
      let
        pertinent =
          filter (\(Transition m start _) -> Bits.testBit s0 start && all (c ==) m) f
      in
        foldl' (.|.) 0 (fmap transitionEndState pertinent)


printC :: Doc -> Regex -> Doc
printC name (Regex _ starting transitions acceptance) =
  vsep [
      "ibool_t iregex_" <> name <> "(const istring_t str) {"
    , indent 2 $ vsep [
        "const char *s = (const istring_t) str;"
      , "uint64_t current = " <> pretty starting <> ";"
      , "while ('\\0' != *s) {"
      , indent 2 $ vsep $ join [
          [ "uint64_t next = 0;" ]
        , fmap goIf transitions
        , [ "current = next;" ]
        ]
      , "s++;"
      , "}"
      , "if (current & " <> pretty acceptance <> ") {"
      , "  return itrue;"
      , " } else {"
      , "  return ifalse;"
      , "}"
      ]
    , "}"
    ]

  where
    goIf (Transition Nothing from to) =
      vsep [
        "if (current & (1 << " <> pretty from <> ")) {"
      , indent 2 $ "next |= " <> pretty to <> ";"
      , "}"
      ]
    goIf (Transition (Just c) from to) =
      vsep [
        "if ((current & (1 << " <> pretty from <> ")) && *s == '" <> pretty c <> "') {"
      , indent 2 $ "next |= " <> pretty to <> ";"
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
