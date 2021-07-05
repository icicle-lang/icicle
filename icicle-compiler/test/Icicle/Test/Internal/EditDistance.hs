{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Internal.EditDistance where

import           Icicle.Test.Arbitrary
import           Icicle.Internal.EditDistance
import           Icicle.Internal.Pretty

import           P

import           System.IO

import           Test.QuickCheck


prop_edit_insertion :: [Char] -> Char -> [Char] -> Property
prop_edit_insertion as i bs =
  editDistance (text as <> text bs) (text as <> char i <> text bs) === 1

prop_edit_symmetric :: [Char] -> [Char] -> Property
prop_edit_symmetric as bs =
  editDistance (text as) (text bs) === editDistance (text bs) (text as)

prop_edit_swap_one :: [Char] -> [Char] -> Char -> Char -> Property
prop_edit_swap_one as bs a b = a /= b ==>
  editDistance (text as <> char a <> text bs) (text as <> char b <> text bs) === 1

prop_edit_flip_middle :: [Char] -> [Char] -> Char -> Char -> Property
prop_edit_flip_middle as bs a b = a /= b ==>
  editDistance (text as <> char a <> char b <> text bs) (text as <> char b <> char a <> text bs) === 1

return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 10)
