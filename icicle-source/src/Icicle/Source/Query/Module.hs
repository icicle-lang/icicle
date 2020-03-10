-- | Top-level queries
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Icicle.Source.Query.Module (
    Module         (..)
  , ModuleName     (..)
  , ModuleImport   (..)
  , ResolvedModule (..)
  , ModuleInfo     (..)
  , ModuleError    (..)

  , getModuleFileName
  , topSort
  ) where


import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Either (EitherT, right, left)
import qualified Data.Graph as Graph
import qualified Data.Text as Text

import           GHC.Generics (Generic)

import           Icicle.Source.Query.Query
import           Icicle.Source.Query.Function

import           P

import           System.FilePath
import           System.Directory


newtype ModuleName =
  ModuleName {
    getModuleName :: Text
  } deriving (Show, Eq, Ord, Generic)


data Module a n =
  Module {
      moduleName     :: ModuleName
    , moduleImports  :: [ModuleImport a]
    , moduleEntries  :: [Decl a n]
    } deriving (Show, Eq, Ord, Generic)


data ResolvedModule a n =
  ResolvedModule {
      resolvedName     :: ModuleName
    , resolvedImports  :: [ModuleImport a]
    , resolvedEntries  :: [ResolvedFunction a n]
    } deriving (Show, Eq, Ord, Generic)



data ModuleImport a =
  ModuleImport {
      importAnn      :: a
    , importName     :: ModuleName
    } deriving (Show, Eq, Ord, Generic)


data ModuleInfo a n =
  ModuleInfo {
      modInfo         :: Module a n
    , modInfoImports  :: [ModuleImport a]
    }


data ModuleError a =
  ModuleNotFound a FilePath
  deriving (Show, Eq, Ord, Generic)



-- | Generate a sorted list of modules, based on their inputs.
--
--   We should be able to type check and inline in order once done.
topSort :: Ord n => [ModuleInfo a n] -> [Module a n]
topSort ms =
  let
    (gr,lu,_) =
      Graph.graphFromEdges
        [(m, moduleName (modInfo m), importName <$> modInfoImports m) | m <- ms]
    lu' v =
      let
        (m,_,_) = lu v
      in
        modInfo m
    sorted =
      lu' <$> Graph.topSort gr
  in
    reverse sorted



-- | Find the file associated with a module
--
--   Both Extras and Extras.icicle are supported
--   as the Module import name.
getModuleFileName
  :: MonadIO m
  => FilePath
  -> ModuleImport a
  -> EitherT (ModuleError a) m FilePath
getModuleFileName parent m = do
  let
    shown =
       Text.unpack . getModuleName $
        importName m

    extended =
      shown <.> "icicle"

    fullPath =
      parent </> extended

  exists <- liftIO (doesFileExist fullPath)

  if exists then right fullPath else left (ModuleNotFound (importAnn m) fullPath)
