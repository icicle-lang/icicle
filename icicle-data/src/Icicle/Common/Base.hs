-- | Base definitions common across all languages.
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Base (
      Name
    , NameBase(..)
    , nameOf
    , nameHash
    , nameBase
    , modName

    , BaseValue(..)
    , StructField(..)
    , ExceptionInfo(..)
    , WindowUnit(..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.NanEq
import              Icicle.Data.Time

import              P

import qualified    Data.Map    as Map
import qualified    Data.Text   as T
import              Data.Hashable

import              GHC.Generics (Generic)


data Name n = Name {
    nameHash :: {-# UNPACK #-} !Int
  , nameBase ::                !(NameBase n)
  } deriving (Show, Generic)

instance Eq n => Hashable (Name n) where
  hash           (Name h _) = h
  hashWithSalt s (Name h _) = hashWithSalt s h

instance Eq n => Eq (Name n) where
  (==) x y = (nameHash x == nameHash y) && (nameBase x == nameBase y)

instance Eq n => Ord (Name n) where
  compare x y = compare (nameHash x) (nameHash y)

instance NanEq n => NanEq (Name n)

-- | User defined names.
data NameBase n =
 -- | Raw name
   NameBase !n
 -- | Prefix a name.
 -- Very useful for generating fresh(ish) readable names.
 | NameMod  !n !(NameBase n)
 deriving (Eq, Ord, Show, Functor, Generic)

instance Hashable n => Hashable (NameBase n)

instance NanEq n => NanEq (NameBase n)

nameOf :: Hashable n => NameBase n -> Name n
nameOf n = Name (hash n) n

modName :: Hashable n => n -> Name n -> Name n
modName prefix = nameOf . NameMod prefix . nameBase


instance NFData n => NFData (NameBase n)
instance NFData n => NFData (Name n)

--------------------------------------------------------------------------------

data WindowUnit
 = Days   !Int
 | Months !Int
 | Weeks  !Int
 deriving (Show, Eq, Ord, Generic)

instance NFData WindowUnit
instance NanEq WindowUnit

-- | Base values - real values that can be serialised and whatnot
-- These are used in the expressions, but actual values can be
-- closures, which include expressions.
-- This is in here to resolve circular dependency.
data BaseValue
 = VInt      !Int
 | VDouble   !Double
 | VUnit
 | VBool     !Bool
 | VTime     !Time
 | VString   !T.Text
 | VArray    ![BaseValue]
 | VPair     !BaseValue !BaseValue
 | VLeft     !BaseValue
 | VRight    !BaseValue
 | VNone
 | VSome     !BaseValue
 | VMap      !(Map.Map BaseValue    BaseValue)
 | VStruct   !(Map.Map StructField  BaseValue)
 | VBuf      ![BaseValue]
 | VError    !ExceptionInfo
 deriving (Show, Ord, Eq, Generic)

instance NFData BaseValue
instance NanEq BaseValue

-- | Called "exceptions"
-- because they aren't really errors,
-- but they aren't really good values either..
data ExceptionInfo
 -- | This particular exception is for packing (SumT ErrorT a) together into (ErrorT, a)
 = ExceptNotAnError
 | ExceptTombstone
 | ExceptFold1NoValue
 | ExceptCannotCompute
 | ExceptNotANumber
 | ExceptIndexOutOfBounds
 deriving (Show, Ord, Eq, Generic, Enum, Bounded)

instance NFData ExceptionInfo
instance NanEq ExceptionInfo

newtype StructField
 = StructField
 { nameOfStructField :: T.Text
 }
 deriving (Ord, Eq, Generic)

instance NFData StructField
instance NanEq StructField

instance Show StructField where
 showsPrec p (StructField x)
  = showParen (p > 10) (showString "StructField " . showsPrec 11 x)

-- Pretty printing ---------------

instance Pretty n => Pretty (Name n) where
 pretty (Name _ n) = pretty n

instance Pretty n => Pretty (NameBase n) where
 pretty (NameBase n)   = pretty n
 pretty (NameMod  p n) = pretty p <> text "-" <> pretty n

instance Pretty BaseValue where
  prettyPrec p = \case
    VInt i ->
      annotate AnnConstant $ pretty i
    VDouble i ->
      annotate AnnConstant $ pretty i
    VUnit ->
      prettyPunctuation "()"
    VBool b ->
      annotate AnnConstant $ pretty b
    VTime t ->
      annotate AnnConstant $ text $ T.unpack $ renderOutputTime t
    VString t ->
      annotate AnnConstant $ text $ show t
    VArray vs ->
      pretty vs
    VPair a b ->
      pretty (a, b)
    VLeft a ->
      prettyApp hsep p (prettyConstructor "Left") [a]
    VRight a ->
      prettyApp hsep p (prettyConstructor "Right") [a]
    VSome a ->
      prettyApp hsep p (prettyConstructor "Some") [a]
    VNone ->
      prettyConstructor "None"
    VMap mv ->
      prettyApp hsep p (prettyConstructor "Map") [Map.toList mv]
    VStruct mv ->
      prettyApp hsep p (prettyConstructor "Struct") [Map.toList mv]
    VError e ->
      pretty e
    VBuf vs ->
      prettyApp hsep p (prettyConstructor "Buf") [vs]

instance Pretty StructField where
  pretty =
     text . T.unpack . nameOfStructField

instance Pretty ExceptionInfo where
  pretty =
    prettyConstructor . show

instance Pretty WindowUnit where
  pretty = \case
    Days i ->
      annotate AnnConstant (pretty i) <+> prettyKeyword "days"
    Months i ->
      annotate AnnConstant (pretty i) <+> prettyKeyword "months"
    Weeks i ->
      annotate AnnConstant (pretty i) <+> prettyKeyword "weeks"
