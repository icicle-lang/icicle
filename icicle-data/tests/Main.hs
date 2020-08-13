{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Main

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text
import qualified Data.List as List
import           Icicle.Data.Regex
import           Jetski

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Either.Exit (orDie)
import           Control.Monad.Morph


prop_match :: Property
prop_match =
  withTests 1 . property $ do
    let
      complex = star dot `times` once 'A' `times` star (once 'B' `times` once 'C') `times` once 'D' `times` star dot
      source = show $ printCWithTestHeaders "test" complex

    annotate source
    x <- evalIO . runEitherT $
      withLibrary [] (Text.pack source) $ \library -> do
        r <- function library "iregex_test" retCInt
        liftIO $
          Char8.useAsCString (Char8.pack "zzzABCBCDzzz") $ \matching' -> r [argPtr matching']

    x === Right 1


prop_long :: Property
prop_long =
  withTests 1 . property $ do
    let
      long = concat . replicate  2 $ take 50 ['a' .. 'z']
      lreg = foldr (times . once) epsilon long
      source = show $ printCWithTestHeaders "test" lreg

    annotate source
    x <- evalIO . runEitherT $
      withLibrary [] (Text.pack source) $ \library -> do
        r <- function library "iregex_test" retCInt
        liftIO $
          Char8.useAsCString (Char8.pack long) $ \matching' -> r [argPtr matching']

    x === Right 1


tests :: IO Bool
tests =
  checkParallel $$(discover)


main :: IO ()
main = defaultMain [tests]
