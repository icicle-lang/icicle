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

    -- * From dictionaires and libraries
  , queryOfSource
  , entryOfQuery
  , readIcicleLibrary

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

import           Control.Monad.Trans.Either

import           Data.Functor.Identity
import qualified Data.Map                                 as M
import           Data.String
import           Data.Hashable                            (Hashable)

import qualified Text.ParserCombinators.Parsec            as Parsec

import           GHC.Generics                             (Generic)

import           P


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
             -> Either Error (Funs Sorbet.Position Var)
sourceParseF env t
 = first ErrorSourceParse
 $ Parse.parseFunctions env t

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


sourceDesugarF :: Funs Sorbet.Position Var
               -> Either (ErrorSource Var) (Funs Sorbet.Position Var)
sourceDesugarF fun
 = runIdentity . runEitherT . bimapEitherT ErrorSourceDesugar snd
 $ Fresh.runFreshT
     (mapM sourceDesugarDecl fun)
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
             -> Funs    Sorbet.Position Var
             -> Either  Error (FunEnvT Sorbet.Position Var)
sourceCheckF env parsedImport
 = first fst
 $ second fst
 $ sourceCheckFunLog env parsedImport

sourceCheckFunLog :: FunEnvT Sorbet.Position Var
                  -> Funs    Sorbet.Position Var
                  -> Either  (Error, [Check.CheckLog Sorbet.Position Var]) (FunEnvT Sorbet.Position Var, [[Check.CheckLog Sorbet.Position Var]])
sourceCheckFunLog env parsedImport
 = first (first ErrorSourceCheck)
 $ snd
 $ flip Fresh.runFresh (freshNamer "check")
 $ runEitherT
 $ Check.checkFs env
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

readIcicleLibrary :: Var -> Parsec.SourceName -> Text -> Either Error (FunEnvT Sorbet.Position Var)
readIcicleLibrary _name source input
 = do input' <- first ErrorSourceParse $ Parse.parseFunctions source input
      let
        env
          = Dict.dictionaryFunctions Dict.emptyDictionary

      sourceCheckF env input'
