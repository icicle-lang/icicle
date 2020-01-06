-- | Pretty for functions
--
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Pretty (
    prettyFunWithNames
  , prettyFunFromStrings
  , letterNames
  ) where


import                  Icicle.Common.Base
import                  Icicle.Source.Type.Base
import                  Icicle.Source.Type.Subst

import                  Icicle.Internal.Pretty

import                  P

import                  Data.String
import                  Data.List (zip)
import qualified        Data.Map as Map
import                  Data.Hashable (Hashable)


-- | This is a rather dodgy trick.
--   Function types are generalised with fresh variable names,
--   however fresh variable names are quite ugly.
--   Instead of actually using nice names, we will clean them up
--   just before pretty printing.
prettyFunWithNames :: (Pretty n, Eq n) => [Name n] -> Scheme n -> PrettyFunType
prettyFunWithNames names fun
  =  prettyFun fun'
  where
   sub
    = Map.fromList
    (schemeBounds fun `zip` fmap TypeVar names)

   fun' = substF sub (fun { schemeBounds = [] })

-- We make them actual names (with the hash code) because they will be used
-- for substituations.
prettyFunFromStrings :: (IsString n, Pretty n, Hashable n, Eq n) => Scheme n -> PrettyFunType
prettyFunFromStrings
 = prettyFunWithNames
 $ fmap (nameOf . NameBase . fromString) letterNames

letterNames :: [String]
letterNames
 =  fmap (\c -> [c]) ['a'..'z']
 <> concatMap (\prefix -> fmap (\c -> prefix <> [c]) ['a'..'z']) letterNames
 