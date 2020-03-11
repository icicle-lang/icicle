{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Arbitrary.Corpus where

import qualified Data.List as List
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import           Test.QuickCheck

import           P

import           Icicle.Common.Base
import qualified Icicle.Common.Type as CT
import           Icicle.Data hiding (inputName)
import           Icicle.Dictionary.Data hiding (inputId, outputId, prelude)
import           Icicle.Internal.Pretty
import           Icicle.Test.Arbitrary.Data
import           Icicle.Test.Arbitrary.Program
import qualified Icicle.Compiler        as Compiler
import qualified Icicle.Compiler.Source as Source
import qualified Icicle.Core as Core
import qualified Icicle.Source.Lexer.Token as T
import qualified Icicle.Source.Query as Query
import qualified Icicle.Storage.Dictionary.Sorbet as DictionaryLoad

import qualified Prelude as Savage


newtype CorpusId =
  CorpusId {
      unCorpusId :: Int
    } deriving (Eq, Ord, Show, Enum, Num)

corpusInputs :: [(InputName, CT.ValType)]
corpusInputs =
 [ ([inputname|int|]
   , CT.IntT)
 , ([inputname|string|]
   , CT.StringT)
 , ([inputname|injury|]
   , CT.StructT . CT.StructType $ Map.fromList [
       (CT.StructField "location", CT.StringT)
     , (CT.StructField "action", CT.OptionT CT.StringT)
     , (CT.StructField "severity",  CT.IntT)
     ]
    )
 ]
--  where
--   optfield name encoding
--    = Data.StructField Data.Optional name encoding
--   field name encoding
--    = Data.StructField Data.Mandatory name encoding

corpusInputId :: InputName -> InputId
corpusInputId = InputId [namespace|corpus|]

corpusQueries :: [(InputName, OutputId, Text)]
corpusQueries =
  [
    -- Tombstones/no values treament between Core and C

    ( [inputname|int|]
    , [outputid|tombstone:bool|]
    , "from int in fold x = False then True in x")

  , ( [inputname|injury|]
    , [outputid|tombstone:group|]
    , "from injury in group location in count location ")

  , ( [inputname|injury|]
    , [outputid|tombstone:group_group|]
    , "from injury in group location in group location in count location ")

  , ( [inputname|injury|]
    , [outputid|tombstone:group_group_group|]
    , "from injury in group location in group location in group location in count location ")

  , ( [inputname|int|]
    , [outputid|tombstone:filter|]
    , "from int in filter tombstone in count value")

  , ( [inputname|injury|]
    , [outputid|tombstone:filter_group|]
    , "from injury in filter tombstone in group location in count location ")

  , ( [inputname|injury|]
    , [outputid|tombstone:filter_group_group|]
    , "from injury in filter tombstone in group location in group location in count location ")

  , ( [inputname|injury|]
    , [outputid|tombstone:filter_group_group_group|]
    , "from injury in filter tombstone in group location in group location in group location in count location ")

    -- Basic queries

  , ( [inputname|int|]
    , [outputid|corpse:int_oldest|]
    , "from int in oldest value")

  , ( [inputname|string|]
    , [outputid|corpse:string_oldest|]
    , "from string in oldest value")

  , ( [inputname|string|]
    , [outputid|corpse:string_count|]
    , "from string in count value")

  , ( [inputname|string|]
    , [outputid|corpse:string_group|]
    , "from string in group value in count value")

  , ( [inputname|string|]
    , [outputid|corpse:string_group_group|]
    , "from string in group value in group value in count value")

  -- Sane Groups

  , ( [inputname|injury|]
    , [outputid|corpse:injury_group_group|]
    , "from injury in group location in group action in sum severity")

  , ( [inputname|injury|]
    , [outputid|corpse:injury_group_group_group|]
    , "from injury in group location in group action in group severity in sum severity")

  , ( [inputname|injury|]
    , [outputid|corpse:injury_group_pair|]
    , "from injury in group (location, action) in sum severity")

  -- CEO Groups

  , ( [inputname|injury|]
    , [outputid|corpse:injury_group_crazy|]
    , Text.unlines
    [ "from injury"
    , "in windowed 7 days"
    , "in let is_homer = location == \"homer\""
    , "in fold x = (map_create, None) then"
    , "     case tombstone of"
    , "       True then"
    , "         (map_create, Some time)"
    , "       False then"
    , "         case snd x of"
    , "           None then"
    , "             case is_homer of"
    , "               True then"
    , "                 (map_insert location severity (fst x), Some time)"
    , "               False then"
    , "                 (fst x, Some time)"
    , ""
    , "           Some t then"
    , "             case time == t of"
    , "               True then"
    , "                 case is_homer of"
    , "                   True then"
    , "                     (map_insert location severity (fst x), Some time)"
    , "                   False then"
    , "                     (fst x, Some time)"
    , ""
    , "               False then"
    , "                 case is_homer of"
    , "                   True then"
    , "                     (map_insert location severity map_create, Some time)"
    , "                   False then"
    , "                     (map_create, Some time)"
    , ""
    , "in (group fold (k, v) = fst x in min v)"
    ])
  ]

genWellTypedFromSource :: (InputName, OutputId, Text) -> Gen WellTyped
genWellTypedFromSource (_inputName, outputId, src) =
  case coreOfSource outputId src of
    Left u ->
      Savage.error . show . vsep $
        [ "Corpus program cannot be converted to Core:"
        , pretty u
        , text . Text.unpack . renderOutputId $ outputId
        , text . Text.unpack $ src
        ]
    Right core ->
      genWellTypedFromCore outputId core

genWellTypedFromCore :: OutputId -> Core.Program () Var -> Gen WellTyped
genWellTypedFromCore outputId core = do
  wta <- tryGenAttributeFromCore' core
  case wta of
   Left err  ->
     Savage.error . show . vsep $
       [ "Generating attribute for corpus failed: "
       , text . Text.unpack . renderOutputId $ outputId
       , pretty err
       ]
   Right w ->
     return w

testAllCorpus :: (CorpusId -> WellTyped -> Property) -> Property
testAllCorpus prop =
  conjoin . with (List.zip [0..] corpusQueries) $ \(cid, query) ->
    forAll (genWellTypedFromSource query) (prop cid)

--------------------------------------------------------------------------------

prelude :: Either Savage.String [DictionaryFunction]
prelude =
  mconcat . fmap Query.resolvedEntries <$>
    nobodyCares (mapM (uncurry $ Source.readIcicleLibrary "check") [DictionaryLoad.prelude])

inputDictionary :: Either Savage.String Dictionary
inputDictionary =
  let
    mkEntry (n, v) =
      DictionaryInput (corpusInputId n) v (Set.singleton tombstone) (InputKey Nothing)
  in
    Dictionary
      (mapOfInputs $ fmap mkEntry corpusInputs)
      (mapOfOutputs [])
      <$> prelude

coreOfSource :: OutputId -> Text -> Either Savage.String (Core.Program () Var)
coreOfSource oid src
 = do d0 <- inputDictionary
      parsed <- nobodyCares $ Source.queryOfSource Source.defaultCheckOptions d0 oid src
      core0 <- nobodyCares $ Compiler.coreOfSource1 Source.defaultCompileOptions d0 parsed
      let core1 = Core.renameProgram varOfVariable core0
      return core1

varOfVariable :: Name T.Variable -> Name Var
varOfVariable = nameOf . fmap (\(T.Variable v) -> Var v 0) . nameBase
