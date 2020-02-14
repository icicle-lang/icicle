{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Icicle.Source.Checker.Error (
    CheckError(..)
  , ErrorInfo(..)
  , ErrorSuggestion(..)
  , annotOfError
  , errorNoSuggestions
  , errorSuggestions
  , errorInFunction
  , errorInFunctionEither
  ) where

import           GHC.Generics (Generic)

import           Icicle.Source.Query
import           Icicle.Source.Type

import           Icicle.Common.Base
import qualified Icicle.Common.Fresh                as Fresh
import           Icicle.Data.Name
import           Icicle.Internal.EditDistance
import           Icicle.Internal.Pretty

import           P

import           Data.List (sortBy, take, intersperse)
import           Data.String
import           Data.Hashable (Hashable)

data CheckError a n
 = CheckError (ErrorInfo a n) [ErrorSuggestion a n]
 deriving (Show, Eq, Generic)

instance (NFData a, NFData n) => NFData (CheckError a n)

data ErrorInfo a n
 = ErrorNoSuchVariable a (Name n)
 | ErrorNoSuchInput a UnresolvedInputId
 | ErrorContextNotAllowedHere  a (Context a n)
 | ErrorFunctionWrongArgs      a (Exp a n) (Scheme n) [Type n]
 | ErrorSchemesMatchError      a (Scheme n) (Scheme n)
 | ErrorApplicationNotFunction a (Exp a n)
 | ErrorConstraintsNotSatisfied a [(a, DischargeError n)]
 | ErrorConstraintLeftover      a [(a, Constraint n)]
 | ErrorCantInferConstraints    a [(a, Constraint n)] [(a, Constraint n)]
 | ErrorReturnNotAggregate      a (Type n)
 | ErrorDuplicateFunctionNames a (Name n)
 | ErrorEmptyCase a (Exp a n)
 | ErrorPartialBinding a (Pattern n)
 | ErrorCaseBadPattern a (Pattern n)
 | ErrorResumableFoldNotAllowedHere a (Query a n)
 | ErrorInFunctionCall a (Name n) (ErrorInfo a n)
 | ErrorImpossible a
 deriving (Show, Eq, Generic)

instance (NFData a, NFData n) => NFData (ErrorInfo a n)

annotOfError :: CheckError a n -> a
annotOfError (CheckError e _)
 = case e of
    ErrorNoSuchVariable a _
     -> a
    ErrorNoSuchInput a _
     -> a
    ErrorContextNotAllowedHere  a _
     -> a
    ErrorFunctionWrongArgs      a _ _ _
     -> a
    ErrorSchemesMatchError      a _ _
     -> a
    ErrorApplicationNotFunction a _
     -> a
    ErrorConstraintsNotSatisfied          a _
     -> a
    ErrorConstraintLeftover a _
     -> a
    ErrorCantInferConstraints a _ _
     -> a
    ErrorReturnNotAggregate          a _
     -> a
    ErrorDuplicateFunctionNames      a _
     -> a
    ErrorEmptyCase          a _
     -> a
    ErrorPartialBinding        a _
     -> a
    ErrorCaseBadPattern          a _
     -> a
    ErrorResumableFoldNotAllowedHere a _
     -> a
    ErrorInFunctionCall a _ _
     -> a
    ErrorImpossible a
     -> a

data ErrorSuggestion a n
 = AvailableFeatures UnresolvedInputId [(InputId, Type n)]
 | AvailableBindings (Name n) [(Name n, Scheme n)]
 | Suggest String
 deriving (Show, Eq, Generic)

instance (NFData a, NFData n) => NFData (ErrorSuggestion a n)


-- Helpers ------------
errorNoSuggestions :: ErrorInfo a n -> Either (CheckError a n) r
errorNoSuggestions info
 = Left $ CheckError info []

errorSuggestions :: ErrorInfo a n -> [ErrorSuggestion a n] -> Either (CheckError a n) r
errorSuggestions info sugs
 = Left $ CheckError info sugs

errorInFunction :: a -> Name n -> CheckError a n -> CheckError a n
errorInFunction ann nm (CheckError err sugs)
 = CheckError (ErrorInFunctionCall ann nm err) sugs

errorInFunctionEither :: a -> Name n -> Either (CheckError a n) r -> Either (CheckError a n) r
errorInFunctionEither a n e
 = first (errorInFunction a n) e

-- Pretties ----------

instance (IsString n, Pretty a, Pretty n, Hashable n, Eq n) => Pretty (CheckError a n) where
  pretty = \case
    CheckError info [] ->
      pretty info
    CheckError info sugs ->
      vsep [
          pretty info
        , mempty
        , vsep . intersperse mempty $
            fmap pretty sugs
        ]

instance (IsString n, Pretty a, Pretty n, Hashable n, Eq n) => Pretty (ErrorInfo a n) where
  pretty = \case
    ErrorNoSuchVariable a n ->
      "Unknown variable" <+> annotate AnnError (pretty n) <+> "at" <+> pretty a

    ErrorNoSuchInput a n ->
      "The dictionary has no input called" <+> annotate AnnError (pretty n) <+> "at" <+> pretty a

    ErrorContextNotAllowedHere a c ->
      vsep [
          "Context is not allowed at" <+> pretty a
        , mempty
        , "Context: " <> inp c
        ]

    ErrorFunctionWrongArgs a x f tys ->
      vsep [
          "Function applied to wrong number of arguments at" <+> pretty a
        , mempty
        , "Expression:     " <> inp x
        , "Function type:  " <> inp (prettyFunFromStrings f)
        , "Argument types: " <> inp tys
        ]

    ErrorSchemesMatchError a x f ->
      vsep [
          "Supplied type signature does not match that of the inferred type at" <+> pretty a
        , mempty
        , "Signature:      " <> inp x
        , "Inferred type:  " <> inp (prettyFunFromStrings f)
        ]

    ErrorApplicationNotFunction a x ->
      vsep [
          "Application of non-function at" <+> pretty a
        , mempty
        , "Exp: " <> inp x
        ]

    ErrorConstraintsNotSatisfied a ds ->
      vsep [
          "Cannot discharge constraints at" <+> pretty a
        , mempty
        , vcat (fmap (\(an,con) -> indent 2 (pretty an) <> indent 2 (pretty con)) ds)
        ]

    ErrorConstraintLeftover a ds ->
      vsep [
          "Unsolved constraints at" <+> pretty a
        , mempty
        , vcat (fmap (\(an,con) -> indent 2 (pretty an) <> indent 2 (pretty con)) ds)
        ]

    ErrorCantInferConstraints a ds [] ->
      vsep [
          "Can't infer required constraints at" <+> pretty a
        , mempty
        , vcat (fmap (\(an,con) -> indent 2 (pretty an) <> indent 2 (pretty con)) ds)
        , mempty
        , "No constraints were specified"
        ]

    ErrorCantInferConstraints a ds xs ->
      vsep [
          "Can't infer required constraints at" <+> pretty a
        , mempty
        , vcat (fmap (\(_,con) -> indent 2 (pretty con)) ds)
        , mempty
        , "from the specified constraints:"
        , mempty
        , vcat (fmap (\(_,con) -> indent 2 (pretty con)) xs)
        ]

    ErrorReturnNotAggregate a t ->
      vsep [
          "Return type is not an aggregate value at" <+> pretty a
        , mempty
        , "Type: " <> inp t
        ]

    ErrorDuplicateFunctionNames a n ->
      "Function" <+> annotate AnnError (pretty n) <+> "at" <+> pretty a <+> "is already defined"

    ErrorEmptyCase a x ->
      vsep [
          "Case expression has no clauses at" <+> pretty a
        , mempty
        , "Exp: " <> inp x
        ]

    ErrorPartialBinding a x ->
      vsep [
         "Binding expression has a partial pattern match at" <+> pretty a
        , mempty
        , "Pattern: " <> inp x
        ]

    ErrorCaseBadPattern a p ->
      vsep [
          "Case expression has ill-formed pattern at" <+> pretty a
        , mempty
        , "Pattern: " <> inp p
        ]

    ErrorResumableFoldNotAllowedHere a q ->
      vsep [
          "For resumable queries, folds, groups and distincts must be inside windowed or latest at" <+> pretty a
        , mempty
        , "Fold: " <> inp q
        ]

    ErrorInFunctionCall a n e' ->
      vsep [
          "In call to" <+> annotate AnnError (pretty n) <+> "at" <+> pretty a <> ":"
        , mempty
        , pretty e'
        ]

    ErrorImpossible a ->
      vsep [
          "Impossible error at " <+> pretty a
        ]
   where
    inp x = align (pretty x)

instance (IsString n, Pretty n, Hashable n, Eq n) => Pretty (ErrorSuggestion a n) where
  pretty = \case
    AvailableFeatures n' bs ->
      let
        bs' =
          take 5 $ flip sortBy bs $ on compare $ (editDistance $ pretty n') . pretty . fst
      in
        vsep [
            "Suggested features are:"
          , mempty
          , indent 2 . vsep . intersperse mempty $
              fmap pretty_ty bs'
          ]

    AvailableBindings n' bs ->
      let
        inb =
          grabInbuilt <$> listOfWiredFuns

        bs' =
          take 5 $ flip sortBy (fmap (first nameBase) bs <> inb) $ on compare $ (editDistance $ pretty n') . pretty . fst

      in
        vsep [
            "Suggested bindings are:"
          , mempty
          , indent 2 . vsep . intersperse mempty $
              fmap pretty_fun_ty bs'
          ]

    Suggest str ->
      text str

   where
    pretty_ty     (k,t) = prettyTypedBest (annotate AnnBinding $ pretty k) (pretty t)
    pretty_fun_ty (k,t) = prettyTypedFun (annotate AnnBinding $ pretty k) (prettyFunFromStrings t)

    grabInbuilt f       = (NameBase . fromString . (flip displayS "") . renderCompact . pretty . Fun $ f, prettyInbuiltType f)

    prettyInbuiltType
     = snd
     . flip Fresh.runFresh freshNamer
     . primLookup'
     . Fun
       where
         freshNamer
          = Fresh.counterPrefixNameState (fromString . show) "inbuilt"
