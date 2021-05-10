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
    Type         (..)
  , Scheme       (..)
  , TraverseType (..)
  , Constraint   (..)
  , Annot        (..)

  , prettyFun
  , typeOfValType
  , valTypeOfType
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



-- | Source language type
--
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
 | TypeArrow           (Type n) (Type n)
 deriving (Eq, Ord, Show, Generic)



-- | A polymorphic type Scheme.
--
--   This represents a type whose bound names can
--   be instantiated with any types which respect
--   the supplied constraints.
data Scheme n
 = Forall {
    schemeBounds      :: [Name n]
  , schemeConstraints :: [Constraint n]
  , schemeType        :: Type n
 } deriving (Eq, Ord, Show, Generic)


anyArrows :: Type n -> Bool
anyArrows
 = let go TypeArrow {} = Any True
       go x = foldSourceType go x
   in getAny . go


class TraverseType a n where
  traverseType :: Applicative f => (Type n -> f (Type n)) -> (a -> f a)


instance TraverseType a n => TraverseType [a] n where
  traverseType f d = traverse (traverseType f) d


instance TraverseType (Type n) n where
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
    TypeArrow a b           -> TypeArrow <$> f a <*> f b

instance TraverseType (Scheme n) n where
  traverseType f (Forall ns cs typ) =
    Forall ns <$> traverseType f cs <*> f typ

foldSourceType :: Monoid x => (Type n -> x) -> (Type n -> x)
foldSourceType =
  foldMapOf traverseType

mapSourceType :: TraverseType a n => (Type n -> Type n) -> a -> a
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
    TypeArrow _ _           -> Nothing
 where
  go = valTypeOfType


data Constraint n
 = CEquals (Type n) (Type n)
 | CIsNum (Type n)
 | CHasField (Type n) (Type n) CT.StructField
 | CPossibilityOfNum (Type n) (Type n)
 | CTemporalityJoin (Type n) (Type n) (Type n)
 | CReturnOfLetTemporalities (Type n) (Type n) (Type n)
 | CDataOfLatest (Type n) (Type n) (Type n) (Type n)
 | CPossibilityOfLatest (Type n) (Type n) (Type n)
 | CPossibilityJoin (Type n) (Type n) (Type n)
 deriving (Eq, Ord, Show, Generic)

instance NFData n => NFData (Type n)
instance NFData n => NFData (Scheme n)
instance NFData n => NFData (Constraint n)

instance TraverseType (Constraint n) n where
  traverseType f t = case t of
    CEquals t1 t2
      -> CEquals <$> f t1 <*> f t2
    CIsNum t1
      -> CIsNum <$> f t1
    CHasField t1 t2 field
      -> CHasField <$> f t1 <*> f t2 <*> pure field
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

instance TraverseType (Annot a n) n where
  traverseType f t = case t of
    Annot a r cs
      -> (\r' -> Annot a r' cs) <$> f r

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
    TypeArrow f x ->
      parensWhenArg p $
        pretty $ PrettyFunType [] [parensWhen (anyArrows f) (pretty f)] (pretty x)

    Temporality a b
      | TypeVar _ <- a ->
        prettyApp hsep p (prettyConstructor "Temporality") [a, b]
      | otherwise ->
        prettyApp hsep p a [b]
    TemporalityPure ->
      prettyConstructor "Pure"
    TemporalityElement ->
      prettyConstructor "Element"
    TemporalityAggregate ->
      prettyConstructor "Aggregate"

    Possibility a b
      | TypeVar _ <- a ->
        prettyApp hsep p (prettyConstructor "Possibility") [a, b]
      | otherwise ->
        prettyApp hsep p a [b]
    PossibilityPossibly ->
      prettyConstructor "Possibly"
    PossibilityDefinitely ->
      prettyConstructor "Definitely"


prettyFun :: Pretty n => Scheme n -> PrettyFunType
prettyFun fun =
  let
    go typ = case typ of
      TypeArrow a res ->
        let
          (as, res') = go res
          a' = if anyArrows a then parens (pretty a) else pretty a
        in
          (a' : as, res')
      _ ->
        ([], pretty typ)

    (args, final) =
      go (schemeType fun)

    cons =
      pretty <$> schemeConstraints fun

  in
    PrettyFunType cons args final


instance Pretty n => Pretty (Scheme n) where
  pretty =
    pretty . prettyFun


instance Pretty n => Pretty (Constraint n) where
  pretty = \case
    CEquals p q ->
      pretty p <+> prettyPunctuation "=:" <+> pretty q

    CIsNum t ->
      prettyConstructor "Num" <+> pretty t

    CHasField ret t f ->
      pretty ret <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "HasField") [pretty t, pretty f]

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

instance Pretty n => Pretty (Annot a n) where
  pretty ann =
    prettyItems sep (pretty $ annResult ann) $
      fmap (PrettyItem (prettyPunctuation "=>") . pretty . snd) (annConstraints ann)
