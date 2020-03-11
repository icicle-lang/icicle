{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , readIcicleModule
  , gatherModules
  , checkModules

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

import           Control.Monad.Trans.Class                (lift)
import           Control.Monad.Trans.Either
import qualified Control.Monad.Trans.State                as State
import           Control.Monad.IO.Class

import qualified Data.ByteString                          as ByteString
import           Data.Functor.Identity
import qualified Data.List                                as List
import qualified Data.Map                                 as M
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
type FunEnvT a b = [ Query.ResolvedFunction a b ]

type QueryUntyped v = Query.QueryTop            Sorbet.Position  v
type QueryTyped   v = Query.QueryTop (TypeAnnot Sorbet.Position) v

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
 | ErrorSourceDesugar     !(Desugar.DesugarError Sorbet.Position var)
 | ErrorSourceCheck       !(Check.CheckError     Sorbet.Position var)
 | ErrorSourceModuleError !(Query.ModuleError    Sorbet.Position)
 deriving (Show, Generic)

-- FIXME We can't implement NFData properly for this type because Parse.ParseError is
-- FIXME not NFData, we really should define our own type for source positions.
instance NFData (ErrorSource a) where rnf x = seq x ()

annotOfError :: ErrorSource a -> Maybe Sorbet.Position
annotOfError e
 = case e of
    ErrorSourceParse _
     -> Nothing
    ErrorSourceDesugar e'
     -> Just (Desugar.annotOfError e')
    ErrorSourceCheck       e'
     -> Just (Check.annotOfError e')
    ErrorSourceModuleError (Query.ModuleNotFound a _)
     -> Just a


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

    ErrorSourceModuleError (Query.ModuleNotFound _ fp) ->
      vsep [
          reannotate AnnErrorHeading $ prettyH2 "Module error"
        , mempty
        , indent 2 $ "Can't find module:" <+> pretty fp
        ]
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
             -> Either Error (Query.Module Sorbet.Position Var)
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
 :: Query.Decl Sorbet.Position Var
 -> Fresh.FreshT Var (EitherT (Desugar.DesugarError Sorbet.Position Var) Identity)
                     (Query.Decl Sorbet.Position Var)
sourceDesugarDecl (Query.DeclFun a ns t x)
 = Query.DeclFun a ns t <$> Desugar.desugarX x


sourceDesugarF :: Query.Module Sorbet.Position Var
               -> Either (ErrorSource Var) (Query.Module Sorbet.Position Var)
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

sourceCheckF :: FunEnvT Sorbet.Position Var
             -> Query.Module  Sorbet.Position Var
             -> Either  Error (Query.ResolvedModule Sorbet.Position Var)
sourceCheckF env parsedImport
 = first fst
 $ second fst
 $ sourceCheckFunLog env parsedImport

sourceCheckFunLog :: FunEnvT Sorbet.Position Var
                  -> Query.Module  Sorbet.Position Var
                  -> Either  (Error, [Check.CheckLog Sorbet.Position Var]) (Query.ResolvedModule Sorbet.Position Var, [[Check.CheckLog Sorbet.Position Var]])
sourceCheckFunLog env parsedImport
 = first (first ErrorSourceCheck)
 $ snd
 $ flip Fresh.runFresh (freshNamer "check")
 $ runEitherT
 $ Check.checkModule env
 $ parsedImport

sourceInline :: Inline.InlineOption
             -> Dictionary
             -> QueryTyped   Var
             -> QueryUntyped Var
sourceInline opt d q
 = Query.reannot Type.annAnnot
 $ inline q
 where
  funs      = M.fromList
            $ fmap (\x -> (Dict.functionName x, Dict.functionDefinition x))
            $ Dict.dictionaryFunctions d
  inline q' = snd
            $ Fresh.runFresh
                (Inline.inlineQT opt funs q')
                (freshNamer "inline")


readIcicleLibrary :: FilePath -> Parsec.SourceName -> Text -> EitherT Error IO (Query.ResolvedModule Sorbet.Position Var)
readIcicleLibrary rootDir source input
 = do current <- hoistWith ErrorSourceParse $ Parse.parseModule source input
      let
        imports =
          Query.moduleImports current

      unchecked <- gatherModules rootDir imports [current]
      checked   <- checkModules unchecked
      return $
        Query.ResolvedModule (Query.moduleName current) imports (snd checked)


readIcicleModule :: FilePath -> Query.ModuleName -> EitherT Error IO ([Query.ResolvedModule Sorbet.Position Var], [Dict.DictionaryFunction])
readIcicleModule rootDir moduleName = do
  unchecked <- gatherModules rootDir [Query.ModuleImport (Sorbet.Position "<implicit>" 1 1) moduleName] []
  checkModules unchecked


-- FIXME
-- Should accumulate a Map, and each check should open its
-- imports instead of using all of them.
checkModules :: [Query.Module Sorbet.Position Var] -> EitherT Error IO ([Query.ResolvedModule Sorbet.Position Var], [Dict.DictionaryFunction])
checkModules unchecked =
  hoistEither $
    flip State.runStateT Dict.builtinFunctions $
      for unchecked $ \m -> do
        env <- State.get
        res <- lift $ sourceCheckF env m
        State.put (Query.resolvedEntries res <> env)
        return res


openIcicleModule :: FilePath -> Query.ModuleImport Sorbet.Position -> EitherT Error IO (Query.Module Sorbet.Position Var)
openIcicleModule rootDir mi = do
  fname <- firstEitherT ErrorSourceModuleError $ Query.getModuleFileName rootDir mi
  input <- Text.decodeUtf8 <$> liftIO (ByteString.readFile fname)
  modul <- hoistWith ErrorSourceParse $ Parse.parseModule fname input
  return modul


-- | Build the module dependency graph.
gatherModules
  :: FilePath
  -> [Query.ModuleImport Sorbet.Position]
  -> [Query.Module Sorbet.Position Var]
  -> EitherT Error IO [Query.Module Sorbet.Position Var]

gatherModules _ [] accum = return $ Query.topSort accum
gatherModules rootDir (m:ms') accum = do
  current <- openIcicleModule rootDir m
  let
    imports =
      Query.moduleImports current
    accum' =
      current : accum
    remaining =
      List.filter (\imp -> Query.importName imp /= Query.importName m) $
        List.nub (ms' <> imports)

  gatherModules rootDir remaining accum'


hoistWith :: Monad m => (a -> c) -> Either a b -> EitherT c m b
hoistWith f = hoistEither . first f
