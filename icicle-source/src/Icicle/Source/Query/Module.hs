-- | Top-level queries
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Icicle.Source.Query.Module (
    Module         (..)
  , ModuleName     (..)
  , ModuleImport   (..)
  , ResolvedModule (..)
  , ModuleError    (..)
  , Decl           (..)

  , getModuleFileName
  , topSort
  , annotOfModuleError
  ) where


import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Either (EitherT, right, left)
import           Data.Array as Array
import qualified Data.Graph as Graph
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Tree (Tree(..))
import qualified Data.Text as Text

import           GHC.Generics (Generic)

import           Icicle.Common.Base (Name)
import           Icicle.Internal.Pretty (Pretty (..), (<+>), encloseSep, prettyPunctuation)

import           Icicle.Source.Type
import           Icicle.Source.Query.Exp
import           Icicle.Source.Query.Query
import           Icicle.Source.Query.Function


import           P

import           System.FilePath
import           System.Directory


data Decl a n
  = DeclFun a (Name n) (Maybe (Scheme n)) (Exp a n)
  deriving (Eq, Ord, Show, Generic)


instance TraverseAnnot Decl where
  traverseAnnot f decl =
    case decl of
      DeclFun a n t x ->
        DeclFun <$> f a <*> pure n <*> pure t <*> traverseAnnot f x


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


data ModuleError a n
  = ModuleNotFound a FilePath
  | ModuleCycles (NonEmpty [Module a n])
  deriving (Show, Eq, Ord, Generic)

instance Pretty n => Pretty (ModuleError a n) where
  pretty = \case
    ModuleNotFound _ fp ->
      "Can't find module:" <+> pretty fp
    ModuleCycles (x :| _) ->
      "Cyclical dependencies discovered" <+>
        encloseSep mempty mempty (prettyPunctuation "->") (fmap (pretty . getModuleName . moduleName) x)

annotOfModuleError :: ModuleError a n -> Maybe a
annotOfModuleError e
 = case e of
    ModuleNotFound a _
     -> Just a
    ModuleCycles _
     -> Nothing

pathsToNode :: Eq a => a -> Tree a -> [[a]]
pathsToNode x (Node y ns) =
  [[x] | x == y] <> fmap (y:) (pathsToNode x =<< ns)

-- | Generate a sorted list of modules, based on their inputs.
--
--   We should be able to type check and inline in order once done.
topSort :: Ord n => [Module a n] -> Either (ModuleError a n) [Module a n]
topSort ms =
  let
    (gr,lu,_) =
      Graph.graphFromEdges
        [(m, moduleName m, importName <$> moduleImports m) | m <- ms]

    lu' v =
      let
        (m,_,_) = lu v
      in
        m

    vertexOutEdges =
      Array.assocs gr

    forests =
      fmap (\(m,is) -> (m, Graph.dfs gr is)) vertexOutEdges

    pathsToSelves = do
      (a, fs) <- forests
      ts      <- fs
      fmap (\path -> lu' <$> (a : path)) $ pathsToNode a ts

    sorted =
      lu' <$> Graph.topSort gr
  in
    case pathsToSelves of
      [] ->
        Right (reverse sorted)
      (x:xs) ->
        Left (ModuleCycles (x :| xs))


-- | Find the file associated with a module
--
--   Module Extras will look for extras.icicle
--   relative to the parent.
getModuleFileName
  :: MonadIO m
  => FilePath
  -> ModuleImport a
  -> EitherT (ModuleError a n) m FilePath
getModuleFileName parent m = do
  let
    shown =
       Text.unpack . Text.toLower . getModuleName $
        importName m

    extended =
      shown <.> "icicle"

    fullPath =
      parent </> extended

  exists <- liftIO (doesFileExist fullPath)

  if exists then right fullPath else left (ModuleNotFound (importAnn m) fullPath)
