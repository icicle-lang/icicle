{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Storage.Dictionary.Common (
    DictionaryImportError (..)
  , ImplicitPrelude (..)
  , loadDictionary'
  , prelude
  ) where

import           Icicle.Data
import           Icicle.Dictionary.Data

import           Icicle.Internal.Pretty                        hiding ((</>))

import qualified Icicle.Compiler.Source                        as P

import qualified Icicle.Sorbet.Parse                           as Sorbet
import qualified Icicle.Sorbet.Position                        as Sorbet

import           Icicle.Source.Checker                         (CheckOptions (..))
import qualified Icicle.Source.Parser                          as SP
import           Icicle.Source.Query                           (QueryTop (..), Query (..), Exp)
import qualified Icicle.Source.Query                           as SQ

import           Icicle.Storage.Dictionary.Data
import           Icicle.Storage.Dictionary.Toml.TomlDictionary

import qualified Control.Exception                             as E
import           Control.Monad.Trans.Either

import qualified Data.Map.Strict                               as Map

import           System.FilePath
import           System.IO

import qualified Data.Set                                      as Set
import qualified Data.Text                                     as T
import qualified Data.Text.IO                                  as T

import qualified Text.Parsec                                   as Parsec

import           P


data DictionaryImportError
  = DictionaryErrorIO          E.SomeException
  | DictionaryErrorParsecTOML  Parsec.ParseError
  | DictionaryErrorCompilation (P.ErrorSource P.Var)
  | DictionaryErrorParse       [DictionaryValidationError]
  | DictionaryErrorSorbet      (Sorbet.ParseError)
  | DictionaryErrorImpossible
  deriving (Show)

type Module  = SQ.Module Sorbet.Position SP.Variable
type FunEnvT = [ ResolvedFunction Sorbet.Position SP.Variable ]


loadDictionary'
  :: CheckOptions
  -> ImplicitPrelude
  -> [DictionaryFunction]
  -> DictionaryConfig
  -> (DictionaryConfig -> FilePath -> EitherT DictionaryImportError IO (DictionaryConfig, [DictionaryInput'], [DictionaryOutput']))
  -> [DictionaryInput]
  -> FilePath
  -> EitherT DictionaryImportError IO Dictionary
loadDictionary' checkOpts impPrelude parentFuncs parentConf grabber parentInputs dictPath = do
  (conf, inputs0', outputs0') <- grabber parentConf dictPath

  let repoPath          = takeDirectory dictPath

  rawImports           <- traverse (readImport repoPath) (fmap T.unpack (configImports conf))
  let prelude'          = if impPrelude == ImplicitPrelude then prelude else []
  parsedImports        <- hoistEither $ traverse (uncurry parseImport) (prelude' <> rawImports)
  importedFunctions    <- loadImports parentFuncs parsedImports

  -- Functions available for virtual features, and visible in sub-dictionaries.
  let availableFunctions = parentFuncs <> importedFunctions

  -- Do a convoluted dance to construct the concrete definitions without the keys
  -- so that we can check the keys before making the actual concrete definitions.
  let inputs' = foldr remakeConcrete [] inputs0'
  let dictUnchecked = Dictionary (mapOfInputs $ fmap snd inputs' <> parentInputs) Map.empty availableFunctions

  outputs <- checkDefs checkOpts dictUnchecked outputs0'
  inputs <-
    hoistEither . forM inputs' $ \(ConcreteKey' k, e@(DictionaryInput a en ts _)) ->
      case k of
       Nothing ->
         pure e
       Just key -> do
         k' <- checkKey checkOpts dictUnchecked a key
         pure $ DictionaryInput a en ts k'

  let loadChapter fp' = loadDictionary' checkOpts
                                        NoImplicitPrelude
                                        availableFunctions
                                        conf
                                        grabber
                                        inputs
                                        (repoPath </> T.unpack fp')

  loadedChapters <- traverse loadChapter (configChapter conf)

  -- Dictionaries loaded after one another can see the functions of previous
  -- dictionaries. So sub-dictionaries imports can use prelude functions.
  -- Export the dictionaries loaded here, and in sub dictionaries (but not
  -- parent functions, as the parent already knows about those).
  let functions = join $ [importedFunctions] <> (dictionaryFunctions <$> loadedChapters)
  let totalinputs = Map.unions $ mapOfInputs inputs : fmap dictionaryInputs loadedChapters
  let totaldefinitions = Map.unions $ mapOfOutputs outputs : fmap dictionaryOutputs loadedChapters

  pure $ Dictionary totalinputs totaldefinitions functions
  where
    remakeConcrete (DictionaryInput' a e t k) cds
      = (k, DictionaryInput a e (Set.fromList (toList t)) unkeyed)
      : cds

readImport :: FilePath -> FilePath -> EitherT DictionaryImportError IO (FilePath, Text)
readImport repoPath fileRel
 = firstEitherT DictionaryErrorIO . EitherT . E.try $ do
     let fileAbs = repoPath </> fileRel
     src <- T.readFile fileAbs
     return (fileRel, src)

parseImport :: FilePath -> Text -> Either DictionaryImportError Module
parseImport path src
 = first DictionaryErrorCompilation (P.sourceParseF path src)

loadImports :: FunEnvT -> [Module] -> EitherT DictionaryImportError IO FunEnvT
loadImports parentFuncs parsedImports
 = hoistEither . first DictionaryErrorCompilation
 $ foldlM (go parentFuncs) [] parsedImports
 where
  go env acc f
   = do -- Run desugar to ensure pattern matches are complete.
        _  <- P.sourceDesugarF f
        -- Type check the function (allowing it to use parents and previous).
        f' <- P.sourceCheckF (env <> acc) f
        -- Return these functions at the end of the accumulator.
        return $ acc <> (SQ.resolvedEntries f')

checkDefs :: CheckOptions
          -> Dictionary
          -> [DictionaryOutput']
          -> EitherT DictionaryImportError IO [DictionaryOutput]
checkDefs checkOpts d defs
 = hoistEither . first DictionaryErrorCompilation
 $ go `traverse` defs
 where
  go (DictionaryOutput' oid q)
   = do  -- Run desugar to ensure pattern matches are complete.
         _             <- P.sourceDesugarQT q
         -- Type check the virtual definition.
         (checked, _)  <- P.sourceCheckQT checkOpts d q
         pure $ DictionaryOutput oid checked

checkKey :: CheckOptions
         -> Dictionary
         -> InputId
         -> Exp Sorbet.Position P.Var
         -> Either DictionaryImportError (InputKey AnnotSource P.Var)
checkKey checkOpts d iid xx = do
  let l = Sorbet.Position "dummy_pos_ctx"  1 1
  let p = Sorbet.Position "dummy_pos_final"  1 1
  let q = QueryTop (QualifiedInput iid) [outputid|dummy_namespace:dummy_output|]
          -- We know the key must be of Pure or Element temporality,
          -- so it's ok to wrap it in a Group.
          (Query   [SQ.Distinct l xx]
          -- The final expression just needs to be Aggregate.
                   (SQ.Prim p (SQ.Lit (SQ.LitInt 0))))

  (checked, _)  <- first DictionaryErrorCompilation $ do
    q'       <- P.sourceDesugarQT q
    (q'', t) <- P.sourceCheckQT checkOpts d q'
    return (P.sourceReifyQT q'', t)

  case contexts . query $ checked of
    SQ.Distinct _ xx' : _
      -> Right . InputKey . Just $ xx'
    _ -> Left DictionaryErrorImpossible

instance Pretty DictionaryImportError where
  pretty = \case
    DictionaryErrorIO          e  -> "IO Exception:" <+> (text . show) e
    DictionaryErrorParsecTOML  e  -> "TOML parse error:" <+> (text . show) e
    DictionaryErrorCompilation e  -> pretty e
    DictionaryErrorParse       es -> "Validation error:" <+> align (vcat (pretty <$> es))
    DictionaryErrorSorbet      e  -> pretty e
    DictionaryErrorImpossible     -> "Impossible!"
