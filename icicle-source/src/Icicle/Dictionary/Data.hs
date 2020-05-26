{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
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
  , prelude
  , mapOfInputs
  , mapOfOutputs
  , unkeyed
  , tombstonesOfDictionary
  , featureMapOfDictionary
  , parseFact
  , prettyDictionarySummary
  ) where

import           Icicle.Data

import qualified Icicle.Common.Fresh                as Fresh
import           Icicle.Common.Base
import           Icicle.Common.Type                 (ValType(..))

import           Icicle.Sorbet.Position             (Range (..), Position (..))
import           Icicle.Source.Query                (QueryTop (..), ResolvedFunction (..))
import qualified Icicle.Source.Query                as SQ
import           Icicle.Source.Lexer.Token
import qualified Icicle.Source.Type                 as ST

import           Icicle.Encoding

import           Icicle.Internal.Pretty

import           Data.FileEmbed                     (embedFile)
import qualified Data.List                          as List
import qualified Data.Text                          as Text
import qualified Data.Text.Encoding                 as Text
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Set                           (Set)
import           Data.String
import           System.FilePath

import           P

type DictionaryFunction
  = ResolvedFunction Range Variable

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
    , outputQuery :: QueryTop (ST.Annot Range Variable) Variable
    } deriving (Eq, Show)

-- | The query is keyed by this "virtual key". Facts (for one entity) are nubbed by this key.
newtype InputKey a n =
  InputKey {
      unInputKey :: Maybe (SQ.Exp a n)
    } deriving (Eq, Show)

type AnnotSource = ST.Annot Range Variable

unkeyed :: InputKey AnnotSource Variable
unkeyed = InputKey Nothing

tombstonesOfDictionary :: Dictionary -> Map InputId (Set Text)
tombstonesOfDictionary =
  fmap inputTombstones . dictionaryInputs

emptyDictionary :: Dictionary
emptyDictionary =
  Dictionary Map.empty Map.empty builtinFunctions

dummyRange :: Range
dummyRange =
  Range
    (Position "builtin" 1 1)
    (Position "builtin" 1 1)

builtinFunctions :: [DictionaryFunction]
builtinFunctions
  = snd
  $ Fresh.runFresh
    (SQ.builtinDefinitions dummyRange)
    (Fresh.counterPrefixNameState (fromString . show) "builtin")

prelude :: (FilePath, Text)
prelude
 = ("prelude.icicle", Text.decodeUtf8 $(embedFile "data/libs/prelude.icicle"))

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
    :: DictionaryInput
    -> [(InputId, SQ.FeatureConcrete () Variable (InputKey AnnotSource Variable))]
  mkFeatureContext (DictionaryInput iid enc _ key)
   = fmap (iid,) (SQ.mkFeatureContext enc key)

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
