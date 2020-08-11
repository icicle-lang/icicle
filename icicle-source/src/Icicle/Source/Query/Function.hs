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
     functionAnn :: a
   , functionName :: Name n
   , functionType :: Scheme n
   , functionDefinition :: Exp (Annot a n) n
   } deriving (Eq, Ord, Show)

-- | Build a function environment containing our builtin functions.
builtinDefinitions :: (IsString n, Hashable n, Eq n) => a -> Fresh.Fresh n [ResolvedFunction a n]
builtinDefinitions a_fresh = do
  traverse (buildResolved a_fresh) listOfIntroducedFuns

-- | Build an individual function from its primitive definition.
buildResolved :: (IsString n, Hashable n) => a -> BuiltinFun -> Fresh.Fresh n (ResolvedFunction a n)
buildResolved a_fresh builtin = do
  typ
    <- primLookup' (Fun builtin)
  let
    name
      = nameOf . NameBase . fromString . show . pretty $ builtin
    annot
      = Annot a_fresh (schemeType typ) []
    prim
      = Prim annot (Fun builtin)
  return $
    ResolvedFunction a_fresh name typ prim


instance Pretty n => Pretty (ResolvedFunction a n) where
  pretty (ResolvedFunction _ n t x) =
    align $
      vsep [
        pretty n <+> prettyPunctuation ":" <+> pretty t
      , pretty n <+> prettyPunctuation "=" <+> pretty x
      ]

  prettyList =
    align . vsep . fmap pretty


instance TraverseAnnot ResolvedFunction where
  traverseAnnot f xx =
    case xx of
      ResolvedFunction a n t q ->
        ResolvedFunction <$> f a <*> pure n <*> pure t <*> traverseAnnot (traverseAnnot f) q
