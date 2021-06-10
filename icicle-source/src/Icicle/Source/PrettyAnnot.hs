{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards     #-}
module Icicle.Source.PrettyAnnot (
    PrettyAnnot(..)
  ) where

import qualified Data.Text as Text

import           Icicle.Data.Name
import           Icicle.Data.Time
import           Icicle.Internal.Pretty
import           Icicle.Source.Query

import           P


newtype PrettyAnnot q =
    PrettyAnnot {
      getPrettyAnnot :: q
    }

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Exp a n)) where
  prettyPrec outer_prec xx =
    wrap $
      case getPrettyAnnot xx of
        Var _ n ->
          annotate AnnVariable (pretty n)

        Nested _ q ->
          pretty (PrettyAnnot q)

        App{}
           -- Operators
           | Just (Op o, _, [x]) <- takePrimApps (getPrettyAnnot xx)
           , FPrefix <- fixity o
           -> pretty o <+> prettyPrec inner_prec (PrettyAnnot x)
           | Just (Op o, _, [x,y]) <- takePrimApps (getPrettyAnnot xx)
           , FInfix _ <- fixity o
           ->  prettyPrec inner_prec_1 (PrettyAnnot x)
           <+> pretty o
           <+> prettyPrec inner_prec_2 (PrettyAnnot y)

        App _ x y ->
          prettyPrec inner_prec_1 (PrettyAnnot x) <+> prettyPrec inner_prec_2 (PrettyAnnot y)

        Prim _ (Lit (LitTime t)) ->
          annotate AnnPrimitive (text $ Text.unpack $ renderTime t)

        Prim _ p ->
          annotate AnnPrimitive (pretty p)

        Lam _ n x ->
          prettyPunctuation "(" <>
          prettyPunctuation "\\" <> pretty n <+> prettyPunctuation "->" <+> prettyPrec appPrec1 (PrettyAnnot x) <>
          prettyPunctuation ")"

        Case _ scrut pats ->
          vsep [
              prettyKeyword "case" <+> pretty (PrettyAnnot scrut) <+> prettyKeyword "of"
            , prettyPunctuation "{"
            , vcat . with pats $ \(p, x) ->
                vsep [
                    indent 2 $ pretty p <+> prettyPunctuation "then"
                  , indent 4 $ pretty (PrettyAnnot x)
                  ] <+> prettyPunctuation ";"
            , prettyPunctuation "}"
            ]

        If _ scrut true false ->
          vsep [
              prettyKeyword "if" <+> pretty (PrettyAnnot scrut) <+> prettyKeyword "then"
            , indent 4 $ pretty (PrettyAnnot true)
            , prettyKeyword "else"
            , indent 4 $ pretty (PrettyAnnot false)
            ]

        Access _ expression field ->
          pretty expression <> "." <> pretty field

        Record _ fields ->
          prettyPunctuation "{" <>
            vsep (with fields $ \(p, x) -> pretty p <> ":" <> pretty (PrettyAnnot x)) <>
          prettyPunctuation "}"
   where
    (inner_prec, assoc) = precedenceOfX (getPrettyAnnot xx)

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

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Context a n)) where
  pretty cc =
    case getPrettyAnnot cc of
      Windowed {} ->
        pretty (getPrettyAnnot cc)
      Latest {} ->
        pretty (getPrettyAnnot cc)
      GroupBy _ x ->
        prettyKeyword "group" <+> align (pretty (PrettyAnnot x))

      GroupFold _ n1 n2 x ->
        vsep [
            prettyKeyword "group fold" <+> pretty (n1, n2) <+> prettyPunctuation "="
          , indent 2 . align $
              pretty (PrettyAnnot x)
          ]

      Distinct _ x ->
        prettyKeyword "distinct" <+> align (pretty (PrettyAnnot x))

      Filter _ x ->
        prettyKeyword "filter" <+> align (pretty (PrettyAnnot x))

      LetFold _ f ->
        vsep [
            annotate AnnKeyword (pretty $ foldType f)
          , indent 2 . align $
              annotate AnnBinding (pretty (foldBind f)) <+> prettyPunctuation ":" <+> pretty (annotOfExp (foldInit f))
          , indent 2 . align $
              annotate AnnBinding (pretty (foldBind f)) <+> prettyPunctuation "="
          , indent 4 . align $
              pretty (PrettyAnnot $ foldInit f)
          , indent 2 . align $
              prettyKeyword "then"
          , indent 4 . align $
              pretty (PrettyAnnot $ foldWork f)
          ]

      Let _ b x ->
        vsep [
            prettyKeyword "let"
          , indent 2 . align $ annotate AnnBinding (pretty b) <+> prettyPunctuation ":" <+> pretty (annotOfExp x)
          , indent 2 . align $ annotate AnnBinding (pretty b) <+> prettyPunctuation "=" <+> pretty (PrettyAnnot x)
          ]

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Query a n)) where
  pretty qq =
    case getPrettyAnnot qq of
      Query cs x ->
        align . prettyItems vsep (align . pretty $ PrettyAnnot x) $
          fmap (PrettyItem (prettyPunctuation "in") . align . pretty . PrettyAnnot) cs

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (QueryTop a n)) where
  pretty qq =
    case getPrettyAnnot qq of
      QueryTop input _ q ->
        vsep [
            prettyKeyword "from" <+> annotate AnnConstant (pretty (show (renderUnresolvedInputId input)))
          , prettyPunctuation "in" <+> align (pretty (PrettyAnnot q))
          ]
