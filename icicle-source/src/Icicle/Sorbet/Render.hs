{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards     #-}
module Icicle.Sorbet.Render (
    PrettySorbet(..)
  ) where

import qualified Data.Text as Text

import           Icicle.Data.Name
import           Icicle.Data.Time
import           Icicle.Internal.Pretty
import           Icicle.Source.Query

import           P


newtype PrettySorbet q =
    PrettySorbet {
      getPrettySorbet :: q
    }

instance (Pretty n) => Pretty (PrettySorbet (Exp a n)) where
  prettyPrec outer_prec xx =
    wrap $
      case getPrettySorbet xx of
        Var _ n ->
          annotate AnnVariable (pretty n)

        Nested _ q ->
          pretty (PrettySorbet q)

        App{}
           -- Operators
           | Just (Op o, _, [x]) <- takePrimApps (getPrettySorbet xx)
           , FPrefix <- fixity o
           -> pretty o <+> prettyPrec inner_prec (PrettySorbet x)
           | Just (Op o, _, [x,y]) <- takePrimApps (getPrettySorbet xx)
           , FInfix _ <- fixity o
           ->  prettyPrec inner_prec_1 (PrettySorbet x)
           <+> pretty o
           <+> prettyPrec inner_prec_2 (PrettySorbet y)

        App _ x y ->
          prettyPrec inner_prec_1 (PrettySorbet x) <+> prettyPrec inner_prec_2 (PrettySorbet y)

        Prim _ (Lit (LitTime t)) ->
          annotate AnnPrimitive (text $ Text.unpack $ renderTime t)

        Prim _ p ->
          annotate AnnPrimitive (pretty p)

        Lam _ n x ->
          prettyPunctuation "(" <>
          prettyPunctuation "\\" <> pretty n <+> prettyPunctuation "->" <+> prettyPrec appPrec1 (PrettySorbet x) <>
          prettyPunctuation ")"

        Case _ scrut pats ->
          vsep [
              prettyKeyword "case" <+> pretty (PrettySorbet scrut) <+> prettyKeyword "of"
            , prettyPunctuation "{"
            , vcat . with pats $ \(p, x) ->
                vsep [
                    indent 2 $ pretty p <+> prettyPunctuation "then"
                  , indent 4 $ pretty (PrettySorbet x)
                  ] <+> prettyPunctuation ";"
            , prettyPunctuation "}"
            ]

        If _ scrut true false ->
          vsep [
              prettyKeyword "if" <+> pretty (PrettySorbet scrut) <+> prettyKeyword "then"
            , indent 4 $ pretty (PrettySorbet true)
            , prettyKeyword "else"
            , indent 4 $ pretty (PrettySorbet false)
            ]

        Access _ expression field ->
          pretty expression <> "." <> pretty field

   where
    (inner_prec, assoc) = precedenceOfX (getPrettySorbet xx)

    -- Precedence of first operator argument
    inner_prec_1
     | AssocLeft <- assoc
     = inner_prec
     | otherwise
     = inner_prec + 1

    -- Precedence of second operator argument
    inner_prec_2
     | AssocRight <- assoc
     = inner_prec
     | otherwise
     = inner_prec + 1

    -- Suppose we have
    --
    --   7   6   7   (precedences)
    -- a * b + c * d
    --
    --        +        6
    --      /   \
    --     *     *     7
    --    / \   / \
    --   a   b c   d
    --
    -- So when pretty printing, the precedence is allowed to increase
    -- without requiring parentheses, but decreasing needs them.
    --
    wrap =
      parensWhen (inner_prec < outer_prec)

instance Pretty n => Pretty (PrettySorbet (Context a n)) where
  pretty cc =
    case getPrettySorbet cc of
      Windowed {} ->
        pretty (getPrettySorbet cc)
      Latest {} ->
        pretty (getPrettySorbet cc)
      GroupBy _ x ->
        prettyKeyword "group" <+> align (pretty (PrettySorbet x))

      GroupFold _ n1 n2 x ->
        vsep [
            prettyKeyword "group fold" <+> pretty (n1, n2) <+> prettyPunctuation "="
          , indent 2 . align $
              pretty (PrettySorbet x)
          ]

      Distinct _ x ->
        prettyKeyword "distinct" <+> align (pretty (PrettySorbet x))

      Filter _ x ->
        prettyKeyword "filter" <+> align (pretty (PrettySorbet x))

      LetFold _ f ->
        vsep [
            annotate AnnKeyword (pretty $ foldType f)
          , indent 2 . align $
              annotate AnnBinding (pretty (foldBind f)) <+> prettyPunctuation "="
          , indent 4 . align $
              pretty (PrettySorbet $ foldInit f)
          , indent 2 . align $
              prettyKeyword "then"
          , indent 4 . align $
              pretty (PrettySorbet $ foldWork f)
          ]

      Let _ b x ->
        vsep [
            prettyKeyword "let"
          , indent 2 . align $ annotate AnnBinding (pretty b) <+> prettyPunctuation "="
          , indent 4 . align $ pretty $ PrettySorbet x
          ]

instance (Pretty n) => Pretty (PrettySorbet (Query a n)) where
  pretty qq =
    case getPrettySorbet qq of
      Query cs x ->
        align . prettyItems vsep (align . pretty $ PrettySorbet x) $
          fmap (PrettyItem (prettyPunctuation "in") . align . pretty . PrettySorbet) cs

instance (Pretty n) => Pretty (PrettySorbet (QueryTop a n)) where
  pretty qq =
    case getPrettySorbet qq of
      QueryTop input _ q ->
        vsep [
            prettyKeyword "from" <+> annotate AnnConstant (pretty (show (renderUnresolvedInputId input)))
          , prettyPunctuation "in" <+> align (pretty (PrettySorbet q))
          ]
