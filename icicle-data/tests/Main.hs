{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import           Hedgehog
import           Hedgehog.Main

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text
import           Data.Traversable (for)
import           Data.These (These (..))
import           Foreign.C.Types (CInt)
import           Icicle.Data.Regex
import           Jetski

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Morph


runRegex :: (MonadTest m, MonadIO m) => Regex -> String -> m (Either JetskiError CInt)
runRegex regex arg = do
  let source = show $ printCWithTestHeaders "test" regex
  annotate source
  evalIO . runEitherT $
    withLibrary [] (Text.pack source) $ \library -> do
      r <- function library "iregex_test" retCInt
      liftIO $
        Char8.useAsCString (Char8.pack arg) $ \matching' -> r [argPtr matching']

runBattery :: (MonadTest m, MonadIO m) => Regex -> [(String, Bool)] -> m ()
runBattery regex examples = do
  let source = show $ printCWithTestHeaders "test" regex
  annotate source
  final <- evalExceptT . hoist evalIO $
    withLibrary [] (Text.pack source) $ \library -> do
      r <- function library "iregex_test" retCInt
      for examples $ \(arg, expect) -> do
        result <- liftIO $ Char8.useAsCString (Char8.pack arg) $ \matching' -> r [argPtr matching']
        return $
          if expect then
            result == 1
          else
            result == 0
  and final === True

prop_match :: Property
prop_match =
  withTests 1 . property $ do
    let
      complex = star dot `times` once 'A' `times` star (once 'B' `times` once 'C') `times` once 'D' `times` star dot

    x <- runRegex complex "zzzABCBCDzzz"
    x === Right 1


prop_long :: Property
prop_long =
  withTests 1 . property $ do
    let
      long = concat . replicate  2 $ take 50 ['a' .. 'z']
      lreg = foldr (times . once) epsilon long

    x <- runRegex lreg long
    x === Right 1


prop_group :: Property
prop_group =
  withTests 1 . property $
    let
      regex = bound (These 3 5) (range 'a' 'z')
    in
      runBattery regex [
        ("ab", False)
      , ("abc", True)
      , ("ABC", False)
      , ("abcde", True)
      , ("abcdef", False)
      ]

prop_alternatives :: Property
prop_alternatives =
  withTests 1 . property $
    let
      regex = plus ((range '0' '9') `add` (range 'A' 'F'))
    in
      runBattery regex [
        ("AB", True)
      , ("AB00", True)
      , ("", False)
      , ("G", False)
      ]



tests :: IO Bool
tests =
  checkParallel $$(discover)


main :: IO ()
main = defaultMain [tests]
