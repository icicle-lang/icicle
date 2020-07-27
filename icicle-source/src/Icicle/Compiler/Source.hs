{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Icicle.Compiler.Source
  ( ErrorSource (..)

  , IsName

  , Var
  , TypeAnnot
  , AnnotUnit

  , QueryUntyped
  , QueryTyped

  , CoreProgramUntyped

  , Funs
  , FunEnvT

    -- * Compiler options
  , IcicleCompileOptions (..)
  , defaultCompileOptions
  , FusionOptions (..)
  , defaultFusionOptions
  , Inline.InlineOption (..)
  , Inline.defaultInline
  , Check.CheckOptions (..)
  , Check.defaultCheckOptions

    -- * From dictionaries and libraries
  , queryOfSource
  , entryOfQuery
  , readIcicleLibrary
  , readIcicleLibraryPure
  , readIcicleModule
  , gatherModules
  , checkModules
  , implicitModuleImports
  , loadedPrelude

    -- * Works on Source programs
  , sourceParseQT
  , sourceParseF
  , sourceDesugarQT
  , sourceDesugarF
  , sourceReifyQT
  , sourceCheckQT
  , sourceCheckF
  , sourceCheckFunLog
  , sourceInline

    -- * Helpers
  , freshNamer
  , annotOfError
  ) where


import           Icicle.Common.Base
import qualified Icicle.Common.Fresh                      as Fresh

import qualified Icicle.Core.Program.Program              as Core

import           Icicle.Data.Name

import           Icicle.Dictionary                        (Dictionary, DictionaryOutput(..))
import qualified Icicle.Dictionary                        as Dict

import           Icicle.Internal.Pretty

import qualified Icicle.Sorbet.Parse                      as Parse
import qualified Icicle.Sorbet.Position                   as Sorbet
import qualified Icicle.Source.Checker                    as Check
import qualified Icicle.Source.Parser                     as Parse
import qualified Icicle.Source.Query                      as Query
import qualified Icicle.Source.Transform.Desugar          as Desugar
import qualified Icicle.Source.Transform.Inline           as Inline
import qualified Icicle.Source.Transform.ReifyPossibility as Reify
import qualified Icicle.Source.Type                       as Type

import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class

import qualified Data.ByteString                          as ByteString
import           Data.Functor.Identity
import qualified Data.List                                as List
import           Data.Map                                 (Map)
import qualified Data.Map                                 as Map
import           Data.String
import           Data.Hashable                            (Hashable)
import qualified Data.Text.Encoding                       as Text

import qualified Text.ParserCombinators.Parsec            as Parsec

import           GHC.Generics                             (Generic)

import           P

import           System.IO


type IsName v = (Hashable v, Eq v, IsString v, Pretty v, Show v, NFData v)

type AnnotUnit = ()

type Var         = Parse.Variable
type TypeAnnot a = Type.Annot a Var

type Funs a b = [ Query.Decl a b ]
type FunEnvT a b = Query.Features () b (Query.InputKey (TypeAnnot a) b)

type QueryUntyped v = Query.QueryTop            Sorbet.Range  v
type QueryTyped   v = Query.QueryTop (TypeAnnot Sorbet.Range) v

type CoreProgramUntyped v = Core.Program AnnotUnit v

type Error = ErrorSource Var

freshNamer :: IsString v => v -> Fresh.NameState v
freshNamer prefix
 = Fresh.counterPrefixNameState (fromString . show) prefix

--------------------------------------------------------------------------------

data IcicleCompileOptions = IcicleCompileOptions
  { icicleInline        :: Inline.InlineOption
  , icicleBigData       :: Check.CheckOptions
  , icicleFusionOptions :: FusionOptions
  }

defaultCompileOptions :: IcicleCompileOptions
defaultCompileOptions = IcicleCompileOptions Inline.defaultInline Check.defaultCheckOptions defaultFusionOptions

data FusionOptions = FusionOptions
  { fusionMaximumPerKernel :: Int
  }

defaultFusionOptions :: FusionOptions
defaultFusionOptions = FusionOptions 100

--------------------------------------------------------------------------------

data ErrorSource var
 = ErrorSourceParse       !Parse.ParseError
 | ErrorSourceDesugar     !(Desugar.DesugarError Sorbet.Range var)
 | ErrorSourceCheck       !(Check.CheckError     Sorbet.Range var)
 | ErrorSourceModuleError !(Query.ModuleError    Sorbet.Range var)
 | ErrorImpossible
 deriving (Show, Generic)

-- FIXME We can't implement NFData properly for this type because Parse.ParseError is
-- FIXME not NFData, we really should define our own type for source positions.
instance NFData (ErrorSource a) where rnf x = seq x ()

annotOfError :: ErrorSource a -> Maybe Sorbet.Range
annotOfError e
 = case e of
    ErrorSourceParse _
     -> Nothing
    ErrorSourceDesugar e'
     -> Just (Desugar.annotOfError e')
    ErrorSourceCheck       e'
     -> Just (Check.annotOfError e')
    ErrorSourceModuleError me
     -> Query.annotOfModuleError me
    ErrorImpossible
     -> Nothing


instance (Hashable a, Eq a, IsString a, Pretty a) => Pretty (ErrorSource a) where
  pretty = \case
    ErrorSourceParse p ->
      vsep [
          reannotate AnnErrorHeading $ prettyH2 "Parse error"
        , mempty
        , indent 2 . pretty $ p
        ]

    ErrorSourceDesugar d ->
      vsep [
          reannotate AnnErrorHeading $ prettyH2 "Desugar error"
        , mempty
        , indent 2 $ pretty d
        ]

    ErrorSourceCheck ce ->
      vsep [
          reannotate AnnErrorHeading $ prettyH2 "Check error"
        , mempty
        , indent 2 $ pretty ce
        ]

    ErrorSourceModuleError me ->
      vsep [
          reannotate AnnErrorHeading $ prettyH2 "Module error"
        , mempty
        , indent 2 $ pretty me
        ]
    ErrorImpossible ->
      "Impossible"
--------------------------------------------------------------------------------

-- * queries

queryOfSource :: Check.CheckOptions
              -> Dictionary
              -> OutputId
              -> Text
              -> Either Error (QueryTyped Var)
queryOfSource checkOpts dict oid src = do
  parsed       <- sourceParseQT oid src
  desugared    <- sourceDesugarQT parsed
  (checked, _) <- sourceCheckQT checkOpts dict desugared
  pure checked

entryOfQuery ::
     Namespace
  -> OutputName
  -> QueryTyped Var
  -> DictionaryOutput
entryOfQuery nsp oname query
  = DictionaryOutput (OutputId nsp oname) query

-- * source

sourceParseQT :: OutputId
              -> Text
              -> Either Error (QueryUntyped Var)
sourceParseQT oid t
 = first ErrorSourceParse
 $ Parse.parseQueryTop oid t

sourceParseF :: Parsec.SourceName
             -> Text
             -> Either Error (Query.Module Sorbet.Range Var)
sourceParseF env t
 = first ErrorSourceParse
 $ Parse.parseModule env t

sourceDesugarQT ::               QueryUntyped Var
                -> Either Error (QueryUntyped Var)
sourceDesugarQT q
 = runIdentity . runEitherT . bimapEitherT ErrorSourceDesugar snd
 $ Fresh.runFreshT
     (Desugar.desugarQT q)
     (freshNamer "desugar_q")


sourceDesugarDecl
 :: Query.Decl Sorbet.Range Var
 -> Fresh.FreshT Var (EitherT (Desugar.DesugarError Sorbet.Range Var) Identity)
                     (Query.Decl Sorbet.Range Var)
sourceDesugarDecl (Query.DeclFun a ns t x)
 = Query.DeclFun a ns t <$> Desugar.desugarX x
sourceDesugarDecl (Query.DeclInput a iid typ k)
 = pure $ Query.DeclInput a iid typ k
sourceDesugarDecl (Query.DeclOutput a oid top)
 = Query.DeclOutput a oid <$> Desugar.desugarQT top


sourceDesugarF :: Query.Module Sorbet.Range Var
               -> Either (ErrorSource Var) (Query.Module Sorbet.Range Var)
sourceDesugarF module'
 = fmap (\ds -> module' { Query.moduleEntries = ds })
 $ runIdentity . runEitherT . bimapEitherT ErrorSourceDesugar snd
 $ Fresh.runFreshT
     (mapM sourceDesugarDecl (Query.moduleEntries module'))
     (freshNamer "desugar_f")


sourceReifyQT :: QueryTyped Var
              -> QueryTyped Var
sourceReifyQT q
 = snd
 $ runIdentity
 $ Fresh.runFreshT
     (Reify.reifyPossibilityQT q)
     (freshNamer "reify")

sourceCheckQT :: Check.CheckOptions
              -> Dictionary
              -> QueryUntyped Var
              -> Either Error (QueryTyped Var, Type.Type Var)
sourceCheckQT opts d q
 = let d' = Dict.featureMapOfDictionary d
   in  first ErrorSourceCheck
     $ snd
     $ flip Fresh.runFresh (freshNamer "check")
     $ runEitherT
     $ Check.checkQT opts d' q

sourceCheckF :: Check.CheckOptions
             -> FunEnvT Sorbet.Range Var
             -> Query.Module  Sorbet.Range Var
             -> Either  Error (Query.ResolvedModule Sorbet.Range Var)
sourceCheckF opts env parsedImport
 = first fst
 $ second fst
 $ sourceCheckFunLog opts env parsedImport

sourceCheckFunLog :: Check.CheckOptions
                  -> FunEnvT Sorbet.Range Var
                  -> Query.Module Sorbet.Range Var
                  -> Either  (Error, [Check.CheckLog Sorbet.Range Var]) (Query.ResolvedModule Sorbet.Range Var, [[Check.CheckLog Sorbet.Range Var]])
sourceCheckFunLog opts env parsedImport
 = first (first ErrorSourceCheck)
 $ snd
 $ flip Fresh.runFresh (freshNamer "check")
 $ runEitherT
 $ Check.checkModule opts env
 $ parsedImport

sourceInline :: Inline.InlineOption
             -> Dictionary
             -> QueryTyped   Var
             -> QueryUntyped Var
sourceInline opt d q
 = Query.reannot Type.annAnnot
 $ inline q
 where
  funs      = Map.fromList
            $ fmap (\x -> (Dict.functionName x, Dict.functionDefinition x))
            $ Dict.dictionaryFunctions d
  inline q' = snd
            $ Fresh.runFresh
                (Inline.inlineQT opt funs q')
                (freshNamer "inline")


loadedPrelude :: Either Error (Query.Module Sorbet.Range Var)
loadedPrelude =
  first ErrorSourceParse $ uncurry (Parse.parseModule) Dict.prelude


readIcicleLibraryPure :: Check.CheckOptions -> FilePath -> Parsec.SourceName -> Text -> Either Error (Query.ResolvedModule Sorbet.Range Var)
readIcicleLibraryPure opts _ source input
 = do current <- first ErrorSourceParse $ Parse.parseModule source input
      prelude <- loadedPrelude
      let
        imports =
          implicitModuleImports current

      for_ imports $ \(Query.ModuleImport a n) ->
        Left $ ErrorSourceModuleError (Query.ModuleNotFound a (show n))

      checked <- checkModules opts [current { Query.moduleImports = imports }, prelude]
      let
        smooshed =
          join $ Query.resolvedEntries <$> Map.elems checked

        inputs =
          Map.unions $ Query.resolvedInputs <$> Map.elems checked

        outputs =
          Map.unions $ Query.resolvedOutputs <$> Map.elems checked

      return $
        Query.ResolvedModule (Query.moduleName current) [] inputs outputs smooshed


readIcicleLibrary :: Check.CheckOptions -> FilePath -> Parsec.SourceName -> Text -> EitherT Error IO (Query.ResolvedModule Sorbet.Range Var)
readIcicleLibrary opts rootDir source input
 = do current <- hoistWith ErrorSourceParse $ Parse.parseModule source input
      prelude <- hoistEither loadedPrelude
      let
        imports =
          implicitModuleImports current

      unchecked <- gatherModules rootDir imports [current { Query.moduleImports = imports }, prelude]
      checked   <- hoistEither $ checkModules opts unchecked
      let
        smooshed =
          join $ fmap Query.resolvedEntries $ Map.elems checked

        inputs =
          Map.unions $ Query.resolvedInputs <$> Map.elems checked

        outputs =
          Map.unions $ Query.resolvedOutputs <$> Map.elems checked

      return $
        Query.ResolvedModule (Query.moduleName current) imports inputs outputs smooshed


readIcicleModule :: Check.CheckOptions -> Sorbet.Range -> FilePath -> Query.ModuleName -> EitherT Error IO (Map Query.ModuleName (Query.ResolvedModule Sorbet.Range Var))
readIcicleModule opts pos rootDir moduleName = do
  prelude    <- hoistEither loadedPrelude
  unchecked  <- gatherModules rootDir [Query.ModuleImport pos moduleName] [prelude]
  hoistEither $ checkModules opts unchecked


checkModules :: Check.CheckOptions -> [Query.Module Sorbet.Range Var] -> Either Error (Map Query.ModuleName (Query.ResolvedModule Sorbet.Range Var))
checkModules opts unchecked =
  let
    checkModule envs m = do
      let
        imports =
          Query.moduleImports m

      env0 <-
        traverse (note ErrorImpossible . flip Map.lookup envs . Query.importName) imports

      let
        featureMapOfResolved ds functions =
          Query.Features
            (Map.fromList $ concatMap mkFeatureContext ds)
            (Map.fromList $ fmap (\x -> (Query.functionName x, Query.functionType x)) functions)
            (Just $ var "now")

        mkFeatureContext (iid, enc, key) =
          fmap (iid,) (Query.mkFeatureContext enc key)

        var =
          nameOf . NameBase . Parse.Variable

        env1 =
          featureMapOfResolved
            (join (fmap (fmap (\(Query.ModuleInput _ n t _) -> (n, t, Query.unkeyed)) . Map.elems . Query.resolvedInputs) env0))
            (Dict.builtinFunctions <> join (fmap Query.resolvedEntries env0))

      checked <-
        sourceCheckF opts env1 m

      return $
        Map.insert (Query.moduleName m) checked envs

  in
    foldM checkModule Map.empty unchecked


openIcicleModule :: FilePath -> Query.ModuleImport Sorbet.Range -> EitherT Error IO (Query.Module Sorbet.Range Var)
openIcicleModule rootDir mi = do
  fname <- firstEitherT ErrorSourceModuleError $ Query.getModuleFileName rootDir mi
  input <- Text.decodeUtf8 <$> liftIO (ByteString.readFile fname)
  modul <- hoistWith ErrorSourceParse $ Parse.parseModule fname input
  return modul


-- | Build the module dependency graph.
gatherModules
  :: FilePath
  -> [Query.ModuleImport Sorbet.Range]
  -> [Query.Module Sorbet.Range Var]
  -> EitherT Error IO [Query.Module Sorbet.Range Var]

gatherModules rootDir imports accum =
  let
    outstanding =
      List.filter (\imp -> all (\a -> Query.importName imp /= Query.moduleName a) accum) imports
  in
    case outstanding of
      [] ->
        hoistWith ErrorSourceModuleError (Query.topSort accum)
      m:ms -> do
        current <- openIcicleModule rootDir m
        let
          currentImports =
            implicitModuleImports current
          accum' =
            current { Query.moduleImports = currentImports } : accum
          remaining =
            ms <> currentImports

        gatherModules rootDir remaining accum'


implicitModuleImports :: Query.Module Sorbet.Range Var -> [Query.ModuleImport Sorbet.Range]
implicitModuleImports m =
  let
    explicit =
      Query.moduleImports m
    preludeName =
      Query.ModuleName "Prelude"
    preludePos =
      Sorbet.Range
        (Sorbet.Position "<implicit>" 1 1)
        (Sorbet.Position "<implicit>" 1 1)
  in
    if Query.moduleName m == preludeName then
      explicit
    else
      Query.ModuleImport preludePos preludeName : explicit


hoistWith :: Monad m => (a -> c) -> Either a b -> EitherT c m b
hoistWith f = hoistEither . first f

-- | Tag a 'Nothing'.
note :: a -> Maybe b -> Either a b
note a Nothing = Left a
note _ (Just b) = Right b
{-# INLINEABLE note #-}
