-- | Subsumption of types.
--
--   Pretty hacky, as it's just for type annotations,
--   not general type inference.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
module Icicle.Source.Checker.Subsumption (
    subsume
  ) where

import                  Icicle.Common.Fresh (Fresh)

import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Error

import                  Icicle.Source.Type
import                  Icicle.Source.Query

import                  Icicle.Internal.Pretty

import                  P

import                  Control.Monad.Trans.Maybe (MaybeT (..))

import qualified        Data.Set as Set
import                  Data.Hashable (Hashable)


metaVars :: (Hashable n, Eq n, Monad m) => a -> Type n -> m (Type n, Type n, GenConstraintSet a n)
metaVars ann t =
  case t of
    TypeForall _ cs t'
      -> return (t', t', (fmap (ann,) cs))
    _ -> return (t, t, [])

-- | Check that the inferred type is at least as polymorphic as the
--   required type, and that all required constraints are specified.
subsume :: (Pretty n, Hashable n, Eq n) => a -> Exp (Annot a n) n -> Type n -> Type n -> Gen a n (Exp (Annot a n) n, Type n)
subsume ann q inf req = do
  let
    err =
      errorNoSuggestions (ErrorConstraintLeftover (ann) (require ann (CEquals inf req)))

  -- Introduce the inferred type.
  (_, skol, inf'c)  <- introForalls ann inf

  -- Introduce the desired type.
  (_, intro, req'c) <- metaVars ann req

  -- Unify the types, with substitutions to turn the inferred
  -- type variables into the desired ones.
  ss                <- hoistMaybeT err $ unifyT skol intro

  -- Discharge the constraints of the inferred type with the
  -- discovered substitutions
  (q', ss', cons)   <- discharge annotOfExp substTX (q, ss, fmap (second (substC ss)) inf'c)

  -- Rewrite the inside of the inner desired type with final substitutions.
  let intro'         = substT ss' intro
  let bindings       = toList $ freeT intro'
  let req'cons       = fmap snd req'c

  -- The set of the constraints the user specified in their type signature
  let givenCons      = Set.fromList req'cons

  -- The inferred constraints which are not in the user's signature
  let unspecified    = filter (\(_, con) -> Set.notMember con givenCons) cons

  -- If there is an inferred constraint which is not specified
  -- we emit an error.
  unless (null unspecified) $
    genHoistEither $
      errorNoSuggestions (ErrorCantInferConstraints ann unspecified req'c)

  -- Piece together the final type.
  let ret            = if null bindings && null req'cons then
                         intro'
                       else
                         TypeForall bindings req'cons intro'

  -- Need to add subsumption of constraints here.
  -- i.e. Is the constraint set of the inferred type covered by the
  -- given type after substitution and discharge.
  return (q', ret)


hoistMaybeT :: Either (CheckError a n) b -> MaybeT (Fresh n) b -> Gen a n b
hoistMaybeT err act = do
  subsumed <- genLiftFresh . runMaybeT $ act
  case subsumed of
    Nothing ->
      genHoistEither err

    Just realised ->
      return realised
