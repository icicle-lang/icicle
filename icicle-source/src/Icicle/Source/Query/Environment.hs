{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
module Icicle.Source.Query.Environment (
    Features (..)
  , FeatureConcrete (..)
  , FeatureContext (..)
  , FeatureVariable (..)
  , FeatureKey (..)

  , envOfFeatureContext
  , envOfFeatureNow
  , typeOfFeatureVariable
  , mkFeatureContext
  ) where

import qualified        Icicle.Common.Exp.Prim.Minimal     as X
import qualified        Icicle.Common.Exp                  as X
import qualified        Icicle.Core                        as C
import                  Icicle.Common.Base
import                  Icicle.Common.Type                 (ValType(..), StructType(..))

import                  Icicle.Data.Name

import qualified        Icicle.Source.Type                 as ST
import                  Icicle.Source.Type                 (Type, Scheme, typeOfValType)
import                  Icicle.Source.Lexer.Token          (Variable (..))

import                  P

import                  Data.Map (Map)
import qualified        Data.Map as Map


data Features a n k
 = Features
 { featuresConcretes :: Map   InputId (FeatureConcrete a n k)
 , featuresFunctions :: Map   (Name n) (Scheme n)
 , featureNow        :: Maybe (Name n)
 }

data FeatureConcrete a n k
 = FeatureConcrete
 { featureConcreteKey     :: k
 , featureConcreteType    :: Type n
 , featureConcreteContext :: FeatureContext a n
 }

data FeatureContext a n
 = FeatureContext
 { featureContextVariables :: Map (Name n) (FeatureVariable a n)
 , featureContextFactTime  :: Name n
 }

data FeatureVariable a n
 = FeatureVariable
 { featureVariableType     :: Type n
 , featureVariableExp      :: C.Exp a n -> C.Exp a n
 , featureVariablePossibly :: Bool
 }

newtype FeatureKey a n = FeatureKey {
   featureKey :: Maybe (C.Exp a n)
 }


envOfFeatureContext :: FeatureContext a n -> Map (Name n) (Type n)
envOfFeatureContext ff
 = Map.map typeOfFeatureVariable
 $ featureContextVariables ff


typeOfFeatureVariable :: FeatureVariable a n -> Type n
typeOfFeatureVariable fv
 = ST.Temporality ST.TemporalityElement
 $ ST.Possibility (if featureVariablePossibly fv then ST.PossibilityPossibly else ST.PossibilityDefinitely)
 $ featureVariableType fv


envOfFeatureNow :: Eq n => Bool -> Maybe (Name n) -> Map (Name n) (Type n)
envOfFeatureNow nowIsPure
 = Map.fromList
 . maybeToList
 . fmap
   (\n -> (n, ST.Temporality tmp $ ST.Possibility ST.PossibilityDefinitely ST.TimeT))
 where
   tmp
    | nowIsPure = ST.TemporalityPure
    | otherwise = ST.TemporalityAggregate


mkFeatureContext
  :: ValType
  -> k
  -> [FeatureConcrete () Variable k]
mkFeatureContext vt key
  = let context (ty, vars)
          = FeatureConcrete key ty (FeatureContext vars (var "time"))
    in  fmap context (go vt)

  where
  -- If a dictionary entry is a concrete definition, create a feature context with
  -- implicit names such as `now`, `value`, struct field names, etc.
  go :: ValType
     -> [(Type Variable, Map (Name Variable) (FeatureVariable () Variable))]
  go enc
   | en@(StructT st@(StructType fs)) <- enc
   = [ ( baseType     $  sumT en
       , Map.fromList $  exps "fields" en
                      <> fmap (go' st) (Map.toList fs)
       )
     ]

   | otherwise
   = let e' = enc
     in [ ( baseType $ sumT e'
          , Map.fromList $ exps "value" e' ) ]

  go' parent (fn, ft)
   = let getsum b   = xgetsum b fn ft parent
         n          = nameOfStructField fn
         (this, n') = (getsum True, n)
     in varOfField this n' ft

  varOfField get fn ft
   = ( var fn, FeatureVariable (baseType ft) get True)

  sumT ty  = SumT ErrorT ty
  baseType = typeOfValType

  xfst t1 t2
   = X.XPrim () (C.PrimMinimal $ X.PrimPair $ X.PrimPairFst t1 t2)
  xsnd t1 t2
   = X.XPrim () (C.PrimMinimal $ X.PrimPair $ X.PrimPairSnd t1 t2)

  xget f t fs
   = X.XPrim () (C.PrimMinimal $ X.PrimStruct $ X.PrimStructGet f t fs)

  xgetsum hasTime f t fs x
   = let e'     = StructT fs
         nVal   = var "_val"
         nErr   = var "_err"
         xval   = X.XVar () nVal
         xcase  = X.XPrim () $ C.PrimFold (C.PrimFoldSum ErrorT e') (SumT ErrorT t)
         xleft  = X.XPrim () $ C.PrimMinimal $ X.PrimConst $ X.PrimConstLeft  ErrorT t
         xright = X.XPrim () $ C.PrimMinimal $ X.PrimConst $ X.PrimConstRight ErrorT t
         xfld   = xget f t fs
         xend   = if hasTime
                  then xfst (SumT ErrorT e') TimeT `xapp` x
                  else x
     in xcase
      `xapp` (X.XLam () nErr ErrorT (xleft `xapp` X.XVar () nErr))
      `xapp` (X.XLam () nVal e'     (xright `xapp` (xfld `xapp` xval)))
      `xapp` xend

  xtomb t1
   = X.XApp () (X.XPrim () (C.PrimMinimal $ X.PrimRelation X.PrimRelationEq $ SumT ErrorT t1))
               (X.XValue () (SumT ErrorT t1) (VLeft $ VError ExceptTombstone))

  xapp
   = X.XApp ()

  exps :: Text -> ValType -> [(Name Variable, FeatureVariable () Variable)]
  exps str e'
   = [ (var str, FeatureVariable (baseType e') (X.XApp () (xfst (sumT e') TimeT)) True)
     , time_as_snd e'
     , true_when_tombstone e' ]

  time_as_snd :: ValType -> (Name Variable, FeatureVariable () Variable)
  time_as_snd e'
   = ( var "time"
     , FeatureVariable (baseType TimeT) (X.XApp () (xsnd (sumT e') TimeT)) False)

  true_when_tombstone :: ValType -> (Name Variable, FeatureVariable () Variable)
  true_when_tombstone e'
   = ( var "tombstone"
     , FeatureVariable (baseType BoolT) (X.XApp () (xtomb e') . X.XApp () (xfst (sumT e') TimeT)) False)

  var :: Text -> Name Variable
  var = nameOf . NameBase . Variable
