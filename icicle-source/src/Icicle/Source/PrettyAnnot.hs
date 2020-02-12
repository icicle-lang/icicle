{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Icicle.Source.PrettyAnnot (
    PrettyAnnot(..)
  ) where

import           Data.String (String)

import           Icicle.Data.Name
import           Icicle.Internal.Pretty
import           Icicle.Source.Query

import           P


newtype PrettyAnnot q =
  PrettyAnnot {
      getPrettyAnnot :: q
    }

prettyAnnot :: Pretty a => a -> Doc
prettyAnnot x =
  prettyPunctuation ":{" <+> align (pretty x) <+> "}"

prettyAnnotK :: Pretty a => String -> a -> Doc
prettyAnnotK keyword x =
  prettyKeyword keyword <> prettyAnnot x

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Exp a n)) where
  pretty xx =
    case getPrettyAnnot xx of
      Var a n ->
        annotate AnnVariable (pretty n) <> prettyAnnot a

      Nested a q ->
        line <> indent 2 (prettyPunctuation "(" <> pretty (PrettyAnnot q) <> prettyPunctuation ")" <> prettyAnnot a)

      App{} ->
        let
          (f, xs) =
            takeApps $ getPrettyAnnot xx
        in
          prettyPunctuation "(" <>
          pretty (PrettyAnnot f) <> " " <>
          hsep (fmap (pretty . PrettyAnnot) xs) <>
          prettyPunctuation ")"

      Prim a (Op o) ->
        prettyPunctuation "(" <>
        annotate AnnPrimitive (pretty o) <>
        prettyPunctuation ")" <>
        annotPrim a (Op o)

      Prim a p ->
        annotate AnnPrimitive (pretty p) <> annotPrim a p

      Lam a n x ->
        prettyPunctuation "(" <>
        prettyPunctuation "\\" <> pretty n <+> prettyAnnot a <+> prettyPunctuation "->" <+> prettyPrec appPrec1 (PrettyAnnot x) <>
        prettyPunctuation ")"

      Case a scrut pats ->
        vsep [
            prettyAnnotK "case" a <+> pretty (PrettyAnnot scrut)
          , vcat . with pats $ \(p, x) ->
              vsep [
                  prettyPunctuation "|" <+> pretty p <+> prettyPunctuation "->"
                , indent 4 $ pretty (PrettyAnnot x)
                ]
          , prettyKeyword "end"
          ]

      If a scrut true false ->
        vsep [
            prettyAnnotK "if" a <+> pretty (PrettyAnnot scrut) <+> prettyAnnotK "then" a
          , indent 4 $ pretty (PrettyAnnot true)
          , prettyAnnotK "else" a
          , indent 4 $ pretty (PrettyAnnot false)
          ]


      Access _ expression field ->
        pretty expression <> "." <> pretty field

   where
    annotPrim a p
     | primRequiresAnnot p
     = prettyAnnot a
     | otherwise
     = mempty

    -- Do not bother annotating monomorphic prims.
    primRequiresAnnot = \case
     Op o -> case o of
      ArithUnary{}    -> True
      ArithBinary{}   -> True
      ArithDouble{}   -> False
      Relation{}      -> True
      LogicalUnary{}  -> False
      LogicalBinary{} -> False
      TimeBinary{}    -> False
      TupleComma      -> True
     Lit{} -> False
     Fun f -> case f of
      BuiltinMath{}   -> False
      BuiltinText{}   -> False
      BuiltinTime{}   -> False
      BuiltinData{}   -> True
      BuiltinArray{}  -> True
      BuiltinMap{}    -> True
     PrimCon c -> case c of
      ConSome         -> False
      ConNone         -> True
      ConTuple        -> True
      ConTrue         -> False
      ConFalse        -> False
      ConLeft         -> True
      ConRight        -> True
      ConError{}      -> False

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Context a n)) where
  pretty cc =
    case getPrettyAnnot cc of
      Windowed a _ _ ->
        prettyAnnotK "windowed" a
      Latest a _ ->
        prettyAnnotK "latest" a
      GroupBy a x ->
        prettyAnnotK "group" a <+> pretty (PrettyAnnot x)
      GroupFold a _ _ x ->
        prettyAnnotK "groupfold" a <+> pretty (PrettyAnnot x)
      Distinct a x ->
        prettyAnnotK "distinct" a <+> pretty (PrettyAnnot x)
      Filter a x ->
        prettyAnnotK "filter" a <+> pretty (PrettyAnnot x)

      -- For fold and let-bindings, the annotation holds the *return* type, but this isn't
      -- particularly interesting to print since it's the same as the rest of the expression.
      -- It's more useful to print the type of the binding, so to get that we need to
      -- dig in and get the annotation of the bound expression.
      LetFold _ f ->
        vsep [
            annotate AnnKeyword (pretty $ foldType f) <+>
              annotate AnnBinding (pretty (foldBind f)) <> prettyAnnot (annotOfExp $ foldInit f)  <+> prettyPunctuation "="
          , indent 2 . align $
              pretty (PrettyAnnot $ foldInit f) <+> prettyPunctuation ":" <+> pretty (PrettyAnnot $ foldWork f)
          ]

      Let _ b x ->
        vsep [
            prettyKeyword "let" <+> annotate AnnBinding (pretty b) <> prettyAnnot (annotOfExp x) <+> prettyPunctuation "="
          , indent 2 . align $
              pretty $ PrettyAnnot x
          ]

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Query a n)) where
  pretty qq =
    case getPrettyAnnot qq of
      Query cs x ->
        align . prettyItems vsep (align . pretty $ PrettyAnnot x) $
          fmap (PrettyItem (prettyPunctuation "~>") . align . pretty . PrettyAnnot) cs

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (QueryTop a n)) where
  pretty qq =
    case getPrettyAnnot qq of
      QueryTop input _ q ->
        vsep [
            prettyKeyword "feature" <+> annotate AnnConstant (pretty (show (renderUnresolvedInputId input)))
          , prettyPunctuation "~>" <+> align (pretty (PrettyAnnot q))
          ]
