-- | Substitution into types.
-- This is very simple because there are no binders inside actual types
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
module Icicle.Source.Type.Subst (
    SubstT
  , substT
  , substC
  , substF
  , compose
  , unifyT
  ) where

import                  Icicle.Common.Base

import                  Icicle.Source.Type.Base
import                  Icicle.Source.Type.Compounds

import                  P

import                  Control.Lens (over)

import qualified        Data.Map as Map
import qualified        Data.Set as Set
import                  Data.Hashable (Hashable)

type SubstT n = Map.Map (Name n) (Type n)


-- | Substitute into a type.
--
--   TODO: Move into a Fresh monad as quantified
--         types can capture.
substT :: Eq n => SubstT n -> Type n -> Type n
substT ss
 = canonT . go ss
 where
  go ss0 = \case
    TypeVar n
      | Just t' <- Map.lookup n ss
      -> t'
    t
      -> mapSourceType (go ss0) t

-- | Substitute into a constraint.
substC :: Eq n => SubstT n -> Constraint n -> Constraint n
substC ss
 = over traverseType (substT ss)


-- | Substitute into a scheme.
substF :: Eq n => SubstT n -> Scheme n -> Scheme n
substF ss
 = over traverseType (substT ss)

-- | Compose two substitutions together, in order.
-- `compose s1 s2` performs s1 then s2.
--
-- If the same name is mentioned in both, s1 takes priority since it happens first:
--
-- > compose {a := T} {a := U}
-- > = {a := T}
--
-- Although if the name is mentioned in the result, s2 must apply to the results
--
-- > compose {a := T a b} {a := U}
-- > = {a := T U b}
--
-- TODO: a quickcheck property would be ideal for this
--
-- > forall (t : Type n) (s1 s2 : Subst n),
-- > substT s2 (substT s1 t) = substT (compose s1 s2) t
--
-- If the substitutions don't mention the same variables at all,
-- the ordering should not matter.
--
-- > fv s1  \cap fv s1 == {}
-- > ==> compose s1 s2 == compose s2 s1
--
compose :: Eq n => SubstT n -> SubstT n -> SubstT n
compose s1 s2
 = Map.map (substT s2) s1
  `Map.union` s2


-- | Attempt to find a substitution that makes the two types equal.
--
-- >     unifyT t1 t2 == Just   s
-- > ==> substT s  t1 == substT s  t2
--
-- >     unifyT t1 t2 == Nothing
-- > ==>           t1 /= t2
--
unifyT :: (Hashable n, Eq n) => Type n -> Type n -> Maybe (SubstT n)
unifyT t1 t2
 = case t1 of
    TypeVar a
     | TypeVar b <- t2
     , a == b
     -> return $ Map.empty

    TypeVar a
     -- Occurs check.
     -- TODO: it would be nice to have a better error message than just
     -- "Could not unify".
     -- Something specifically about recursive types would be ideal.
     | a `Set.member` freeT t2
     -> Nothing
     | otherwise
     -> return $ Map.singleton a t2
    _
     | TypeVar b <- t2
     , b `Set.member` freeT t1
     -> Nothing
     | TypeVar b <- t2
     -> return $ Map.singleton b t1


    BoolT       -> eq
    TimeT       -> eq
    DoubleT     -> eq
    IntT        -> eq
    StringT     -> eq
    UnitT       -> eq
    ErrorT      -> eq

    ArrayT a
     | ArrayT b <- t2
     -> unifyT a b
     | otherwise
     -> Nothing

    GroupT ak av
     | GroupT bk bv <- t2
     -> compose <$> unifyT ak bk <*> unifyT av bv
     | otherwise
     -> Nothing

    OptionT a
     | OptionT b <- t2
     -> unifyT a b
     | otherwise
     -> Nothing

    PairT a1 a2
     | PairT b1 b2 <- t2
     -> compose <$> unifyT a1 b1 <*> unifyT a2 b2
     | otherwise
     -> Nothing

    SumT  a1 a2
     | SumT  b1 b2 <- t2
     -> compose <$> unifyT a1 b1 <*> unifyT a2 b2
     | otherwise
     -> Nothing

    StructT as
     | StructT bs <- t2
     , Map.keysSet as == Map.keysSet bs
     , m' <- Map.intersectionWith (,) as bs
     ->  foldl compose Map.empty
     <$> mapM (uncurry unifyT) m'
     | otherwise
     -> Nothing

    Temporality at ar
     | Temporality bt br <- t2
     -> compose <$> unifyT at bt <*> unifyT ar br
     | otherwise
     -> Nothing

    TemporalityPure         -> eq
    TemporalityElement      -> eq
    TemporalityAggregate    -> eq

    Possibility at ar
     | Possibility bt br <- t2
     -> compose <$> unifyT at bt <*> unifyT ar br
     | otherwise
     -> Nothing

    PossibilityPossibly     -> eq
    PossibilityDefinitely   -> eq

    TypeArrow  at ar
     | TypeArrow bt br <- t2
     -> compose <$> unifyT at bt <*> unifyT ar br
     | otherwise
     -> Nothing

 where
  eq
   | t1 == t2
   = Just Map.empty
   | otherwise
   = Nothing
