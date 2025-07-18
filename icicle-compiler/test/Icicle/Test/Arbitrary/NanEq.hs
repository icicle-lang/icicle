{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Test.Arbitrary.NanEq (
    (=~=)
  , hedgehogNanEq
  ) where

import           Icicle.Common.NanEq

import           Test.QuickCheck.Property (Property, counterexample)

import qualified Hedgehog
import           Hedgehog.Internal.Show (renderValueDiff, valueDiff)
import qualified Hedgehog.Internal.Source as Hedgehog

import           P

import           Text.Show.Pretty (ppShow)
import qualified Text.Show.Pretty as Pretty

--------------------------------------------------------------------------------
-- QuickCheck

infix 4 =~=

(=~=) :: (NanEq a, Show a) => a -> a -> Property
(=~=) x0 y0 =
  let
    render =
      case (Pretty.reify x0, Pretty.reify y0) of
        (Just x, Just y) ->
          renderValueDiff (valueDiff x y)
        _ ->
          ppShow x0 <>
          " =/= " <>
          ppShow y0
  in
    counterexample "=== Not NaN equal ===" $
    counterexample render (nanEq x0 y0)

infix 4 `hedgehogNanEq`

hedgehogNanEq :: (NanEq a, Show a, Hedgehog.MonadTest m, Hedgehog.HasCallStack) => a -> a -> m ()
hedgehogNanEq =
  flip Hedgehog.diff nanEq
