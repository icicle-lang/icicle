{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion
import Criterion.Main
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text
import Icicle.Data.Regex
import Jetski

import Control.Monad.IO.Class
import Control.Monad.Trans.Either.Exit (orDie)

main :: IO ()
main = orDie (Text.pack . show) $ do
  let
    matching = "ABCBCBCBCD"
    small = replicate 1000 'A'
    large = replicate 1000000 'A'
    early = matching <> large
    complex = star dot `times` once 'A' `times` star (once 'B' `times` once 'C') `times` once 'D' `times` star dot
    source = Text.pack . show $ printCWithTestHeaders "test" complex

  withLibrary [] source $ \library -> do
    r <- function library "iregex_test" retCInt

    liftIO $
      Char8.useAsCString (Char8.pack matching) $ \matching' ->
      Char8.useAsCString (Char8.pack small)    $ \small'  ->
      Char8.useAsCString (Char8.pack large)    $ \large'  ->
      Char8.useAsCString (Char8.pack early)    $ \early'  -> do

      defaultMain [
          bgroup "complex" [
            bench "matching" $ whnfIO (r [argPtr matching'])
          , bench "small" $ whnfIO (r [argPtr small'])
          , bench "large" $ whnfIO (r [argPtr large'])
          , bench "early" $ whnfIO (r [argPtr early'])
          ]
        ]
