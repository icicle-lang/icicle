-- | Helpers on Types
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Compounds (
    freeT
  , freeC
  , canonT
  , decomposeT
  , recomposeT
  , getBaseType
  , getTemporality
  , getPossibility
  , getTemporalityOrPure
  , getPossibilityOrDefinitely

  , anyArrows
  ) where


import                  Icicle.Common.Base
import                  Icicle.Source.Type.Base

import                  P

import                  Control.Lens.Fold      (foldMapOf)
import                  Data.List (zipWith, zip)
import qualified        Data.Map as Map
import qualified        Data.Set as Set
import                  Data.Hashable (Hashable)


freeT :: (Hashable n, Eq n) => Type n -> Set.Set (Name n)
freeT t
 = case t of
    TypeVar n             -> Set.singleton n
    TypeForall ns cs b    -> Set.difference (Set.union (Set.unions (fmap freeC cs)) (freeT b)) (Set.fromList ns)
    _                     -> foldSourceType freeT t


freeC :: (Hashable n, Eq n) => Constraint n -> Set.Set (Name n)
freeC
 = foldMapOf traverseType freeT


canonT :: Type n -> Type n
canonT
 = recomposeT . decomposeT


decomposeT :: Type n -> (Maybe (Type n), Maybe (Type n), Type n)
decomposeT t
 = let tmp  = getTemporality t
       tmpR = maybe t snd tmp
       pos  = getPossibility tmpR
       posR = maybe tmpR snd pos
   in (fst <$> tmp, fst <$> pos, posR)

recomposeT :: (Maybe (Type n), Maybe (Type n), Type n) -> Type n
recomposeT (tmp,pos,dat)
 = maybe id Temporality tmp
 $ maybe id Possibility pos
 $ dat


getTemporality :: Type n -> Maybe (Type n, Type n)
getTemporality tt
 = case tt of
    BoolT         -> Nothing
    TimeT         -> Nothing
    DoubleT       -> Nothing
    IntT          -> Nothing
    StringT       -> Nothing
    UnitT         -> Nothing
    ErrorT        -> Nothing

    ArrayT  a     -> wrap  go ArrayT  a
    GroupT  a b   -> wrap2 go GroupT a b
    OptionT a     -> wrap  go OptionT a
    PairT   a b   -> wrap2 go PairT  a b
    SumT    a b   -> wrap2 go SumT   a b
    StructT fs    -> let fs' = Map.toList fs
                         ks  = fmap fst fs'
                         vs  = fmap snd fs'
                     in  wrapN go (\t vs' -> Just (t, StructT $ Map.fromList (ks `zip` vs'))) vs

    Temporality a b
     -> case go b of
         Just (_,b') -> Just (a, b')
         Nothing     -> Just (a, b)

    TemporalityPure       -> Nothing
    TemporalityElement    -> Nothing
    TemporalityAggregate  -> Nothing

    Possibility a b       -> wrap2 go Possibility a b
    PossibilityPossibly   -> Nothing
    PossibilityDefinitely -> Nothing

    TypeVar {}            -> Nothing
    TypeForall {}         -> Nothing
    TypeArrow {}          -> Nothing

 where
  go = getTemporality


getTemporalityOrPure :: Type n -> Type n
getTemporalityOrPure t
 = case getTemporality t of
    Just (a,_) -> a
    Nothing -> TemporalityPure


getPossibility :: Type n -> Maybe (Type n, Type n)
getPossibility tt
 = case tt of
    BoolT         -> Nothing
    TimeT         -> Nothing
    DoubleT       -> Nothing
    IntT          -> Nothing
    StringT       -> Nothing
    UnitT         -> Nothing
    ErrorT        -> Nothing

    ArrayT  a     -> wrap  go ArrayT a
    GroupT  a b   -> wrap2 go GroupT a b
    OptionT a     -> wrap  go OptionT a
    PairT   a b   -> wrap2 go PairT  a b
    SumT    a b   -> wrap2 go SumT   a b
    StructT fs    -> let fs' = Map.toList fs
                         ks  = fmap fst fs'
                         vs  = fmap snd fs'
                     in  wrapN go (\t vs' -> Just (t, StructT $ Map.fromList (ks `zip` vs'))) vs

    Temporality a b       -> wrap2 go Temporality a b
    TemporalityPure       -> Nothing
    TemporalityElement    -> Nothing
    TemporalityAggregate  -> Nothing

    Possibility a b
     -> case go b of
         Just (_,b') -> Just (a, b')
         Nothing     -> Just (a, b)

    PossibilityPossibly   -> Nothing
    PossibilityDefinitely -> Nothing

    TypeVar {}            -> Nothing
    TypeForall {}         -> Nothing
    TypeArrow {}          -> Nothing

 where
  go = getPossibility

getPossibilityOrDefinitely :: Type n -> Type n
getPossibilityOrDefinitely t
 = case getPossibility t of
    Just (a,_) -> a
    Nothing    -> PossibilityDefinitely

getBaseType :: Type n -> Maybe (Type n)
getBaseType tt
 = case tt of
    BoolT         -> Just tt
    TimeT         -> Just tt
    DoubleT       -> Just tt
    IntT          -> Just tt
    StringT       -> Just tt
    UnitT         -> Just tt
    ErrorT        -> Just tt
    ArrayT  _     -> Just tt
    GroupT  _ _   -> Just tt
    OptionT _     -> Just tt
    PairT   _ _   -> Just tt
    SumT    _ _   -> Just tt
    StructT _     -> Just tt

    Temporality _ t       -> getBaseType t
    TemporalityPure       -> Nothing
    TemporalityElement    -> Nothing
    TemporalityAggregate  -> Nothing

    Possibility _ t       -> getBaseType t
    PossibilityPossibly   -> Nothing
    PossibilityDefinitely -> Nothing

    TypeVar {}            -> Nothing
    TypeForall {}         -> Nothing
    TypeArrow {}          -> Nothing

-- Temporality and possibility helpers --

wrap :: (Type n -> Maybe (Type n, Type n)) -> (Type n -> Type n) -> Type n -> Maybe (Type n, Type n)
wrap go f a
 = case go a of
    Just (tmp,typ) -> Just (tmp, f typ)
    Nothing        -> Nothing

wrap2 :: (Type n -> Maybe (Type n, Type n)) -> (Type n -> Type n -> Type n) -> Type n -> Type n -> Maybe (Type n, Type n)
wrap2 go f a b
 = wrapN go (wrap2' f) [a,b]

wrap2' :: (Type n -> Type n -> Type n) -> Type n -> [Type n] -> Maybe (Type n, Type n)
wrap2' f t xs
 = case xs of
    [a',b'] -> Just (t, f a' b')
    _       -> Nothing

wrapN :: (Type n -> Maybe (Type n, Type n)) -> (Type n -> [Type n] -> Maybe (Type n, Type n)) -> [Type n] -> Maybe (Type n, Type n)
wrapN go f ts
 = let res = fmap go ts
       tmp  = listToMaybe $ fmap fst $ catMaybes res
       args = zipWith (\t r -> fromMaybe t (snd <$> r)) ts res
   in case tmp of
       Nothing -> Nothing
       Just tmp' -> f tmp' args
