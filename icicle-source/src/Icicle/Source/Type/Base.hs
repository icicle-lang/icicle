-- | Source programs have different types than Core and Avalanche:
-- In Source, we need to infer which stage of the computation,
-- so each type is tagged with a universe describing the stage.
--
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
module Icicle.Source.Type.Base (
    Type        (..)
  , TraverseType (..)
  , typeOfValType
  , valTypeOfType
  , Constraint  (..)
  , Annot (..)
  , annotDiscardConstraints
  , foldSourceType
  , mapSourceType
  , anyArrows
  ) where

import           Control.Lens.Fold      (foldMapOf)
import           Control.Lens.Setter    (over)

import           Data.Monoid (Any (..))
import qualified Data.Map as Map

import           GHC.Generics (Generic)

import           Icicle.Common.Base
import qualified Icicle.Common.Type as CT

import           Icicle.Internal.Pretty

import           P


data Type n
 = BoolT
 | TimeT
 | DoubleT
 | IntT
 | StringT
 | UnitT
 | ErrorT

 | ArrayT   (Type n)
 | GroupT   (Type n) (Type n)
 | OptionT  (Type n)
 | PairT    (Type n) (Type n)
 | SumT     (Type n) (Type n)
 | StructT  (Map.Map CT.StructField (Type n))

 | Temporality         (Type n) (Type n)
 | TemporalityPure
 | TemporalityElement
 | TemporalityAggregate

 | Possibility         (Type n) (Type n)
 | PossibilityPossibly
 | PossibilityDefinitely

 | TypeVar             (Name n)
 | TypeForall          [Name n] [Constraint n] (Type n)
 | TypeArrow           (Type n) (Type n)
 deriving (Eq, Ord, Show, Generic)

instance NFData n => NFData (Type n)


anyArrows :: Type n -> Bool
anyArrows
 = let go (TypeArrow {}) = Any True
       go x = foldSourceType go x
   in getAny . go


class TraverseType a where
  type N a :: *
  traverseType :: Applicative f => (Type (N a) -> f (Type (N a))) -> (a -> f a)

instance TraverseType a => TraverseType [a] where
  type N [a] = N a
  traverseType f d = traverse (traverseType f) d

instance TraverseType (Type n) where
  type N (Type n) = n
  traverseType f t = case t of
    BoolT        -> pure BoolT
    TimeT        -> pure TimeT
    DoubleT      -> pure DoubleT
    IntT         -> pure IntT
    StringT      -> pure StringT
    UnitT        -> pure UnitT
    ErrorT       -> pure ErrorT
    ArrayT a     -> ArrayT  <$> f a
    GroupT k v   -> GroupT  <$> f k <*> f v
    OptionT a    -> OptionT <$> f a
    PairT a b    -> PairT   <$> f a <*> f b
    SumT  a b    -> SumT    <$> f a <*> f b
    StructT st   -> StructT <$> traverse f st

    Temporality x a         -> Temporality <$> f x <*> f a
    TemporalityPure         -> pure TemporalityPure
    TemporalityElement      -> pure TemporalityElement
    TemporalityAggregate    -> pure TemporalityAggregate

    Possibility p a         -> Possibility <$> f p <*> f a
    PossibilityPossibly     -> pure PossibilityPossibly
    PossibilityDefinitely   -> pure PossibilityDefinitely

    TypeVar v               -> pure (TypeVar v)
    TypeForall ns cs x      -> TypeForall ns <$> traverseType f cs <*> f x
    TypeArrow a b           -> TypeArrow <$> f a <*> f b

foldSourceType :: Monoid x => (Type n -> x) -> (Type n -> x)
foldSourceType =
  foldMapOf traverseType

mapSourceType :: (Type n -> Type n) -> (Type n -> Type n)
mapSourceType =
  over traverseType

typeOfValType :: CT.ValType -> Type n
typeOfValType vt
 = case vt of
    CT.BoolT        -> BoolT
    CT.TimeT        -> TimeT
    CT.DoubleT      -> DoubleT
    CT.IntT         -> IntT
    CT.StringT      -> StringT
    CT.UnitT        -> UnitT
    CT.ErrorT       -> ErrorT

    CT.ArrayT a     -> ArrayT (go a)
    CT.BufT  _ a    -> ArrayT (go a)
    CT.MapT  k v    -> GroupT (go k) (go v)
    CT.OptionT a    -> OptionT (go a)
    CT.PairT a b    -> PairT (go a) (go b)
    CT.SumT  a b    -> SumT  (go a) (go b)
    CT.StructT st   -> StructT (Map.map go $ CT.getStructType st)
 where
  go = typeOfValType

valTypeOfType :: Type n -> Maybe CT.ValType
valTypeOfType bt
 = case bt of
    BoolT        -> return CT.BoolT
    TimeT        -> return CT.TimeT
    DoubleT      -> return CT.DoubleT
    IntT         -> return CT.IntT
    StringT      -> return CT.StringT
    UnitT        -> return CT.UnitT
    ErrorT       -> return CT.ErrorT
    ArrayT a     -> CT.ArrayT  <$> go a
    GroupT k v   -> CT.MapT    <$> go k <*> go v
    OptionT a    -> CT.OptionT <$> go a
    PairT a b    -> CT.PairT   <$> go a <*> go b
    SumT  a b    -> CT.SumT    <$> go a <*> go b
    StructT st   -> (CT.StructT . CT.StructType)
                <$> traverse go st

    Temporality _ a         -> go a
    TemporalityPure         -> Nothing
    TemporalityElement      -> Nothing
    TemporalityAggregate    -> Nothing

    Possibility _ a         -> go a
    PossibilityPossibly     -> Nothing
    PossibilityDefinitely   -> Nothing

    TypeVar _               -> Nothing
    TypeForall _ _ _        -> Nothing
    TypeArrow _ _           -> Nothing
 where
  go = valTypeOfType


data Constraint n
 = CEquals (Type n) (Type n)
 | CIsNum (Type n)
 | CPossibilityOfNum (Type n) (Type n)
 | CTemporalityJoin (Type n) (Type n) (Type n)
 | CReturnOfLetTemporalities (Type n) (Type n) (Type n)
 | CDataOfLatest (Type n) (Type n) (Type n) (Type n)
 | CPossibilityOfLatest (Type n) (Type n) (Type n)
 | CPossibilityJoin (Type n) (Type n) (Type n)
 deriving (Eq, Ord, Show, Generic)

instance NFData n => NFData (Constraint n)

instance TraverseType (Constraint n) where
  type N (Constraint n) = n
  traverseType f t = case t of
    CEquals t1 t2
      -> CEquals <$> f t1 <*> f t2
    CIsNum t1
      -> CIsNum <$> f t1
    CPossibilityOfNum t1 t2
      -> CPossibilityOfNum <$> f t1 <*> f t2
    CTemporalityJoin t1 t2 t3
      -> CTemporalityJoin <$> f t1 <*> f t2 <*> f t3
    CReturnOfLetTemporalities t1 t2 t3
      -> CReturnOfLetTemporalities <$> f t1 <*> f t2 <*> f t3
    CDataOfLatest t1 t2 t3 t4
      -> CDataOfLatest <$> f t1 <*> f t2 <*> f t3 <*> f t4
    CPossibilityOfLatest t1 t2 t3
      -> CPossibilityOfLatest <$> f t1 <*> f t2 <*> f t3
    CPossibilityJoin t1 t2 t3
      -> CPossibilityJoin <$> f t1 <*> f t2 <*> f t3

data Annot a n
 = Annot
 { annAnnot         :: a
 , annResult        :: Type n
 , annConstraints   :: [(a, Constraint n)]
 }
 deriving (Eq, Ord, Show, Generic)

instance (NFData a, NFData n) => NFData (Annot a n)


annotDiscardConstraints :: Annot a n -> (a, Type n)
annotDiscardConstraints ann
 = (annAnnot ann, annResult ann)



instance Pretty n => Pretty (Type n) where
  prettyPrec p = \case
    IntT ->
      prettyConstructor "Int"
    DoubleT ->
      prettyConstructor "Double"
    UnitT ->
      prettyConstructor "Unit"
    ErrorT ->
      prettyConstructor "ErrorT"
    BoolT ->
      prettyConstructor "Bool"
    TimeT ->
      prettyConstructor "Time"
    StringT ->
      prettyConstructor "String"
    ArrayT t ->
      prettyApp hsep p (prettyConstructor "Array") [t]
    GroupT k v ->
      prettyApp hsep p (prettyConstructor "Group") [k, v]
    OptionT a ->
      prettyApp hsep p (prettyConstructor "Option") [a]
    PairT a b ->
      parens $
        pretty a <> annotate AnnPunctuation (text ",") <+> pretty b
    SumT a b ->
      prettyApp hsep p (prettyConstructor "Sum") [a, b]
    StructT fs ->
      prettyStructType hcat . fmap (bimap pretty pretty) $ Map.toList fs
    TypeVar v ->
      annotate AnnVariable (pretty v)
    TypeForall _ cs x ->
      parensWhenArg p $
        pretty $ PrettyFunType (fmap pretty cs) [] (pretty x)
    TypeArrow f x ->
      parensWhenArg p $
          pretty $ PrettyFunType [] [parensWhen (anyArrows f) (pretty f)] (pretty x)
    Temporality a b ->
      prettyApp hsep p a [b]
    TemporalityPure ->
      prettyConstructor "Pure"
    TemporalityElement ->
      prettyConstructor "Element"
    TemporalityAggregate ->
      prettyConstructor "Aggregate"

    Possibility a b ->
      prettyApp hsep p a [b]
    PossibilityPossibly ->
      prettyConstructor "Possibly"
    PossibilityDefinitely ->
      prettyConstructor "Definitely"


instance Pretty n => Pretty (Constraint n) where
  pretty = \case
    CEquals p q ->
      pretty p <+> prettyPunctuation "=:" <+> pretty q

    CIsNum t ->
      prettyConstructor "Num" <+> pretty t

    CPossibilityOfNum ret t ->
      pretty ret <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "PossibilityOfNum") [t]

    CTemporalityJoin ret a b ->
      pretty ret <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "TemporalityJoin") [a, b]

    CReturnOfLetTemporalities t def body ->
      pretty t <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "ReturnOfLet") [def, body]

    CDataOfLatest t tmp pos dat ->
      pretty t <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "DataOfLatest") [tmp, pos, dat]

    CPossibilityOfLatest t tmp dat ->
      pretty t <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "PossibilityOfLatest") [tmp, dat]

    CPossibilityJoin a b c ->
      pretty a <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "PossibilityJoin") [b, c]

instance (Pretty n) => Pretty (Annot a n) where
  pretty ann =
    prettyItems sep (pretty $ annResult ann) $
      fmap (PrettyItem (prettyPunctuation "=>") . pretty . snd) (annConstraints ann)
