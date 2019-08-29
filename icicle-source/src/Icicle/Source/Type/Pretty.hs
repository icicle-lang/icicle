-- | Pretty for functions
--
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Pretty (
    prettyFunWithNames
  , prettyFunWithLetters
  , prettyFunFromStrings
  , letterNames
  ) where


import                  Icicle.Common.Base
import                  Icicle.Source.Type.Base
import                  Icicle.Source.Type.Subst
import                  Icicle.Source.Lexer.Token

import                  Icicle.Internal.Pretty

import                  P

import                  Data.String
import                  Data.List (splitAt, zip)
import qualified        Data.Map as Map
import qualified        Data.Text as T
import                  Data.Hashable (Hashable)


-- | This is a rather dodgy trick.
--   Function types are generalised with fresh variable names,
--   however fresh variable names are quite ugly.
--   Instead of actually using nice names, we will clean them up
--   just before pretty printing.
prettyFunWithNames :: (Pretty n, Eq n) => [Name n] -> Type n -> PrettyFunType
prettyFunWithNames names typ
 = case typ of
    TypeArrow f typ0 ->
      let
        PrettyFunType consX argsX typ1 =
          prettyFunWithNames names typ0

        mParens =
          if (anyArrows f) then
            parens
          else
            id

      in
        PrettyFunType consX (mParens (pretty $ prettyFunWithNames names f) : argsX) (pretty typ1)

    TypeForall ns cons0 typ0 ->
      let
        (names0, namesRest) =
          splitAt (length ns) names
        sub =
          Map.fromList (ns `zip` fmap TypeVar names0)
        cons1 =
          fmap (substC sub) cons0
        typ1 =
          substT sub typ0
        PrettyFunType consX argsX typ2 =
          prettyFunWithNames namesRest typ1
      in
        PrettyFunType (fmap pretty cons1 <> consX) argsX (pretty typ2)
    x ->
      PrettyFunType [] [] (pretty x)

-- We make them actual names (with the hash code) because they will be used
-- for substituations.
prettyFunFromStrings :: (IsString n, Pretty n, Hashable n, Eq n) => Type n -> PrettyFunType
prettyFunFromStrings
 = prettyFunWithNames
 $ fmap (nameOf . NameBase . fromString) letterNames

prettyFunWithLetters :: Type Variable -> PrettyFunType
prettyFunWithLetters
 = prettyFunWithNames
 $ fmap (nameOf . NameBase . Variable . T.pack) letterNames

letterNames :: [String]
letterNames
 =  fmap (\c -> [c]) ['a'..'z']
 <> concatMap (\prefix -> fmap (\c -> prefix <> [c]) ['a'..'z']) letterNames
