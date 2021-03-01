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

import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Error

import                  Icicle.Source.Type
import                  Icicle.Source.Query

import                  Icicle.Internal.Pretty

import                  P

import qualified        Data.Set as Set
import qualified        Data.Map as Map
import                  Data.Hashable (Hashable)


visitForalls
  :: (Hashable n, Eq n)
  => a
  -> Scheme n
  -> Gen a n (Scheme n, Type n, GenConstraintSet a n)
visitForalls ann f
 = case f of
    Forall _ cs x -> do
      return (f, x, fmap (ann,) cs)


-- | Check that the inferred type is at least as polymorphic as the
--   required type, and that all required constraints are specified.
subsume :: (Pretty n, Hashable n, Eq n) => a -> Exp (Annot a n) n -> Scheme n -> Scheme n -> Gen a n (Exp (Annot a n) n, Scheme n)
subsume ann q inf req = do
  let
    err =
      errorNoSuggestions (ErrorSchemesMatchError ann req inf)

  -- Introduce the inferred type.
  (_, skol, inf'c)  <- visitForalls ann inf

  -- Introduce the desired type.
  (_, intro, req'c) <- visitForalls ann req

  -- Unify the types, with substitutions to turn the inferred
  -- type variables into the desired ones.
  ss                <- hoistMaybe err $ subsumeT skol intro

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
  let ret            = Forall bindings req'cons intro'

  -- Need to add subsumption of constraints here.
  -- i.e. Is the constraint set of the inferred type covered by the
  -- given type after substitution and discharge.
  return (q', ret)


hoistMaybe :: Either (CheckError a n) b -> Maybe b -> Gen a n b
hoistMaybe err act = do
  case act of
    Nothing ->
      genHoistEither err

    Just realised ->
      return realised


-- | Attempt to find a substitution that turns the inferred type
--   into the given type.
--
--   This is very similar to unifyT, in that it attempts to find
--   a substitution which will make the two types the same; but it
--   is heavily biased. Only TypeVars in the inferred type can
--   be cast to concrete types, not ones from the signature.
--
--   Furthermore, when combining substitutions, we must ensure that
--   all cast values are the same (actually, a subsumption might
--   suffice, we'll see).
--
subsumeT :: (Hashable n, Eq n) => Type n -> Type n -> Maybe (SubstT n)
subsumeT inferred sig
 = case inferred of
    TypeVar a
     | TypeVar b <- sig
     , a == b
     -> return Map.empty

    TypeVar a
     -- Occurs check.
     -- TODO: it would be nice to have a better error message than just
     -- "Could not unify".
     -- Something specifically about recursive types would be ideal.
     | a `Set.member` freeT sig
     -> Nothing
     | otherwise
     -> return $ Map.singleton a sig

    BoolT       -> eq
    TimeT       -> eq
    DoubleT     -> eq
    IntT        -> eq
    StringT     -> eq
    UnitT       -> eq
    ErrorT      -> eq

    ArrayT a
     | ArrayT b <- sig
     -> subsumeT a b
     | otherwise
     -> Nothing

    GroupT ak av
     | GroupT bk bv <- sig
     -> join $ combine <$> subsumeT ak bk <*> subsumeT av bv
     | otherwise
     -> Nothing

    OptionT a
     | OptionT b <- sig
     -> subsumeT a b
     | otherwise
     -> Nothing

    PairT a1 a2
     | PairT b1 b2 <- sig
     -> join $ combine <$> subsumeT a1 b1 <*> subsumeT a2 b2
     | otherwise
     -> Nothing

    SumT  a1 a2
     | SumT  b1 b2 <- sig
     -> join $ combine <$> subsumeT a1 b1 <*> subsumeT a2 b2
     | otherwise
     -> Nothing

    StructT as
     | StructT bs <- sig
     , Map.keysSet as == Map.keysSet bs
     , m' <- Map.intersectionWith (,) as bs
     -> join $ foldM combine Map.empty <$> mapM (uncurry subsumeT) m'
     | otherwise
     -> Nothing

    Temporality at ar
     | Temporality bt br <- sig
     -> join $ combine <$> subsumeT at bt <*> subsumeT ar br
     | otherwise
     -> Nothing

    TemporalityPure         -> eq
    TemporalityElement      -> eq
    TemporalityAggregate    -> eq

    Possibility at ar
     | Possibility bt br <- sig
     -> join $ combine <$> subsumeT at bt <*> subsumeT ar br
     | otherwise
     -> Nothing

    PossibilityPossibly     -> eq
    PossibilityDefinitely   -> eq

    TypeArrow at ar
     | TypeArrow bt br <- sig
     -> join $ combine <$> subsumeT at bt <*> subsumeT ar br
     | otherwise
     -> Nothing

 where
  eq
   | inferred == sig
   = Just Map.empty
   | otherwise
   = Nothing

  combine :: Eq n => SubstT n -> SubstT n -> Maybe (SubstT n)
  combine s1 s2 = do
    guard . Map.foldl' (&&) True $ Map.intersectionWith (==) s1 s2
    return $
      Map.union s1 s2
