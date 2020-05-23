{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE FlexibleInstances #-}
module Icicle.Dictionary.Data (
    Dictionary(..)
  , DictionaryInput(..)
  , DictionaryOutput(..)
  , DictionaryFunction
  , ResolvedFunction(..)
  , InputKey(..)
  , AnnotSource
  , emptyDictionary
  , builtinFunctions
  , mapOfInputs
  , mapOfOutputs
  , unkeyed
  , tombstonesOfDictionary
  , featureMapOfDictionary
  , parseFact
  , prettyDictionarySummary
  ) where

import           Icicle.Data

import qualified Icicle.Common.Exp.Prim.Minimal     as X
import qualified Icicle.Common.Exp                  as X
import qualified Icicle.Common.Fresh                as Fresh
import qualified Icicle.Core                        as X
import           Icicle.Common.Base
import           Icicle.Common.Type                 (ValType(..), StructType(..))

import           Icicle.Sorbet.Position             (Position (..))
import           Icicle.Source.Query                (QueryTop (..), ResolvedFunction (..), FeatureVariable (..))
import qualified Icicle.Source.Query                as SQ
import           Icicle.Source.Lexer.Token
import qualified Icicle.Source.Type                 as ST

import           Icicle.Encoding

import           Icicle.Internal.Pretty

import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.String

import           P

type DictionaryFunction
  = ResolvedFunction Position Variable

data Dictionary =
  Dictionary {
      dictionaryInputs :: Map InputId DictionaryInput
    , dictionaryOutputs :: Map OutputId DictionaryOutput
    , dictionaryFunctions :: [DictionaryFunction]
    } deriving (Eq, Show)

data DictionaryInput =
  DictionaryInput {
      inputId :: InputId
    , inputEncoding :: ValType
    , inputTombstones :: Set Text
    , inputKey :: InputKey AnnotSource Variable
    } deriving (Eq, Show)

data DictionaryOutput =
  DictionaryOutput {
      outputId :: OutputId
    , outputQuery :: QueryTop (ST.Annot Position Variable) Variable
    } deriving (Eq, Show)

-- | The query is keyed by this "virtual key". Facts (for one entity) are nubbed by this key.
newtype InputKey a n =
  InputKey {
      unInputKey :: Maybe (SQ.Exp a n)
    } deriving (Eq, Show)

type AnnotSource = ST.Annot Position Variable

unkeyed :: InputKey AnnotSource Variable
unkeyed = InputKey Nothing

tombstonesOfDictionary :: Dictionary -> Map InputId (Set Text)
tombstonesOfDictionary =
  fmap inputTombstones . dictionaryInputs

emptyDictionary :: Dictionary
emptyDictionary =
  Dictionary Map.empty Map.empty builtinFunctions

builtinFunctions :: [DictionaryFunction]
builtinFunctions
  = snd
  $ Fresh.runFresh
    (SQ.builtinDefinitions (Position "builtin" 1 1))
    (Fresh.counterPrefixNameState (fromString . show) "builtin")


mapOfInputs :: [DictionaryInput] -> Map InputId DictionaryInput
mapOfInputs =
  Map.fromList . fmap (\x -> (inputId x, x))

mapOfOutputs :: [DictionaryOutput] -> Map OutputId DictionaryOutput
mapOfOutputs =
  Map.fromList . fmap (\x -> (outputId x, x))

--------------------------------------------------------------------------------

parseFact :: Dictionary -> Fact' -> Either DecodeError Fact
parseFact (Dictionary { dictionaryInputs = dict }) fact'
 = do   def <- maybeToRight
                 (DecodeErrorNotInDictionary attr)
                 (P.find (\(DictionaryInput (InputId _ attr') _ _ _) -> (==) attr attr') dict)
        case def of
         DictionaryInput _ vt ts _ -> do
           enc <- maybe (Left (DecodeErrorNonJsonInput vt)) Right $ encodingOfSourceType vt
           factOf <$> parseValue enc ts (factValue' fact')

 where
  attr = factAttribute' fact'

  factOf v
   = Fact
    { factEntity    = factEntity'    fact'
    , factAttribute = factAttribute' fact'
    , factValue     = v
    }

-- | Get all the features and facts from a dictionary.
--
featureMapOfDictionary :: Dictionary -> SQ.Features () Variable (InputKey AnnotSource Variable)
featureMapOfDictionary (Dictionary { dictionaryInputs = ds, dictionaryFunctions = functions })
 = SQ.Features
     (Map.fromList $ concatMap mkFeatureContext ds)
     (Map.fromList $ fmap (\x -> (functionName x, functionType x)) functions)
     (Just $ var "now")
 where

  mkFeatureContext
   = let context (attr, key, ty, vars)
           = (attr, SQ.FeatureConcrete key ty (SQ.FeatureContext vars (var "time")))
     in  fmap context . go

  -- If a dictionary entry is a concrete definition, create a feature context with
  -- implicit names such as `now`, `value`, struct field names, etc.
  go :: DictionaryInput -> [( InputId
                            , InputKey AnnotSource Variable
                            , ST.Type Variable
                            , Map (Name Variable) (FeatureVariable () Variable))]
  go (DictionaryInput iid enc _ key)
   | en@(StructT st@(StructType fs)) <- enc
   = [ ( iid
       , key
       , baseType     $  sumT en
       , Map.fromList $  exps "fields" en
                      <> fmap (go' st) (Map.toList fs)
       )
     ]

   | otherwise
   = let e' = enc
     in [ ( iid
          , key
          , baseType $ sumT e'
          , Map.fromList $ exps "value" e' ) ]

  go' parent (fn, ft)
   = let getsum b   = xgetsum b fn ft parent
         n          = nameOfStructField fn
         (this, n') = (getsum True, n)
     in varOfField this n' ft

  varOfField get fn ft
   = ( var fn, SQ.FeatureVariable (baseType ft) get True)

  sumT ty  = SumT ErrorT ty
  baseType = ST.typeOfValType

  xfst t1 t2
   = X.XPrim () (X.PrimMinimal $ X.PrimPair $ X.PrimPairFst t1 t2)
  xsnd t1 t2
   = X.XPrim () (X.PrimMinimal $ X.PrimPair $ X.PrimPairSnd t1 t2)

  xget f t fs
   = X.XPrim () (X.PrimMinimal $ X.PrimStruct $ X.PrimStructGet f t fs)

  xgetsum hasTime f t fs x
   = let e'     = StructT fs
         nVal   = var "_val"
         nErr   = var "_err"
         xval   = X.XVar () nVal
         xcase  = X.XPrim () $ X.PrimFold (X.PrimFoldSum ErrorT e') (SumT ErrorT t)
         xleft  = X.XPrim () $ X.PrimMinimal $ X.PrimConst $ X.PrimConstLeft  ErrorT t
         xright = X.XPrim () $ X.PrimMinimal $ X.PrimConst $ X.PrimConstRight ErrorT t
         xfld   = xget f t fs
         xend   = if hasTime
                  then xfst (SumT ErrorT e') TimeT `xapp` x
                  else x
     in xcase
      `xapp` (X.XLam () nErr ErrorT (xleft `xapp` X.XVar () nErr))
      `xapp` (X.XLam () nVal e'     (xright `xapp` (xfld `xapp` xval)))
      `xapp` xend

  xtomb t1
   = X.XApp () (X.XPrim () (X.PrimMinimal $ X.PrimRelation X.PrimRelationEq $ SumT ErrorT t1))
               (X.XValue () (SumT ErrorT t1) (VLeft $ VError ExceptTombstone))

  xapp
   = X.XApp ()

  exps :: Text -> ValType -> [(Name Variable, FeatureVariable () n)]
  exps str e'
   = [ (var str, SQ.FeatureVariable (baseType e') (X.XApp () (xfst (sumT e') TimeT)) True)
     , time_as_snd e'
     , true_when_tombstone e' ]

  time_as_snd :: ValType -> (Name Variable, FeatureVariable () n)
  time_as_snd e'
   = ( var "time"
     , SQ.FeatureVariable (baseType TimeT) (X.XApp () (xsnd (sumT e') TimeT)) False)

  true_when_tombstone :: ValType -> (Name Variable, FeatureVariable () n)
  true_when_tombstone e'
   = ( var "tombstone"
     , SQ.FeatureVariable (baseType BoolT) (X.XApp () (xtomb e') . X.XApp () (xfst (sumT e') TimeT)) False)

  var :: Text -> Name Variable
  var = nameOf . NameBase . Variable

prettyDictionarySummary :: Dictionary -> Doc
prettyDictionarySummary dict =
  vsep [
      prettyH2 "Functions"
    , mempty
    , indent 2 . pprList $
        (pprInbuilt <$> SQ.listOfWiredFuns) <>
        (pprFun <$> dictionaryFunctions dict)
    , mempty
    , prettyH2 "Inputs"
    , mempty
    , indent 2 . pprList $
        fmap pprInput .
        Map.elems $ dictionaryInputs dict
    , mempty
    , prettyH2 "Outputs"
    , mempty
    , indent 2 . pprList $
        fmap pprOutput .
        Map.elems $ dictionaryOutputs dict
    , mempty
    ]
 where
  pprList = \case
    [] ->
      prettyPunctuation "<none>"
    xs ->
      vsep $ List.intersperse mempty xs

  pprInput (DictionaryInput attr enc _ (InputKey mkey)) =
    case mkey of
      Nothing ->
        prettyTypedBest'
          (annotate AnnBinding $ pretty attr)
          (pretty enc)
          (pretty enc)
      Just key ->
        prettyTypedBest'
          (annotate AnnBinding (pretty attr) <+> prettyKeyword "by" <+> annotate AnnVariable (pretty key))
          (pretty enc)
          (pretty enc)

  pprOutput (DictionaryOutput attr q)
   = prettyBinding (pretty attr) $ pretty q

  pprFun (ResolvedFunction f t _)
   = prettyTypedFun (pretty f) (ST.prettyFunFromStrings t)

  pprInbuilt f
   = prettyTypedFun (pretty f) (prettyInbuiltType f)

  prettyInbuiltType
   = ST.prettyFunFromStrings
   . snd
   . flip Fresh.runFresh freshNamer
   . SQ.primLookup'
   . SQ.Fun
     where
       freshNamer
        = Fresh.counterPrefixNameState (Variable . Text.pack . show) "inbuilt"

instance Pretty (InputKey AnnotSource Variable) where
 pretty (InputKey Nothing)  = ""
 pretty (InputKey (Just x)) = "(" <> pretty x <> ")"
