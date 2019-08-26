{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Icicle.Source.Query.Function (
    ResolvedFunction (..)
  , builtinDefinitions
  ) where

import                  Data.Hashable
import                  Data.String

import                  Icicle.Common.Base
import qualified        Icicle.Common.Fresh         as Fresh

import                  Icicle.Internal.Pretty
import                  Icicle.Source.Query.Builtin
import                  Icicle.Source.Query.Prim
import                  Icicle.Source.Query.Query
import                  Icicle.Source.Query.Exp
import                  Icicle.Source.Type

import                  P

data ResolvedFunction a n =
  ResolvedFunction {
     functionName :: Name n
   , functionType :: Type n
   , functionDefinition :: Exp (Annot a n) n
   } deriving (Eq, Show)

-- | Build a function environment containing our builtin functions.
builtinDefinitions :: (IsString n, Hashable n, Eq n) => a -> Fresh.Fresh n [ResolvedFunction a n]
builtinDefinitions a_fresh = do
  traverse (buildResolved a_fresh) listOfIntroducedFuns

-- | Build an individual function from its primitive definition.
--
--   This is a little bit tricky, as we can't under apply function
--   definitions, so we need to create the arguments to the function
--   as well. It's as if we wrote something like this in the prelude
--   @
--   sin x = sin# x
--   @
--   and then type checked it.
buildResolved :: (IsString n, Hashable n) => a -> BuiltinFun -> Fresh.Fresh n (ResolvedFunction a n)
buildResolved a_fresh builtin = do
  typ
    <- primLookup' (Fun builtin)
  let
    name
      = nameOf . NameBase . fromString . show . pretty $ builtin
    annot
      = Annot a_fresh typ []
    prim
      = Prim annot (Fun builtin)
  return $
    ResolvedFunction name typ prim
