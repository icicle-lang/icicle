{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse   #-}
module Icicle.LSP.Task.Hover (
    hover
  ) where

import           P

import           Data.IORef
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Text as Text
import           Data.Aeson as Aeson

import           Data.Monoid (getFirst)

import           Icicle.LSP.State
import           Icicle.LSP.Interface
import           Icicle.LSP.Protocol.Base
import           Icicle.Internal.Pretty
import           Icicle.Source.Type as Query
import           Icicle.Source.Query as Query
import           Icicle.Sorbet.Position (Range (..), Position (..))
import           Icicle.Source.Lexer.Token (Variable)

import           System.IO
import           System.FilePath (takeFileName)

hover :: State -> Text -> JsonRpcId -> Int -> Int -> IO ()
hover state sUri jrid line_ col = do
  let
    localPath =
      fromMaybe (Text.unpack sUri) $
        List.stripPrefix "file://" $
          Text.unpack sUri

  known <- readIORef (stateCoreChecked state)

  case Map.lookup localPath known of
    Just module_ -> do
      lspLog state ("* Cache hit for " <> localPath)

      sendHover state jrid $
        typeAt (Position (takeFileName localPath) (line_ + 1) (col + 1)) module_

    Nothing ->
      sendNothing state jrid

-- findRange :: (TraverseAnnot q, Monoid (f (Type n)), Applicative f) => Position -> q (Annot Range n) n -> Const (f (Type n)) (q a' n)
findRange :: (Applicative f, Monoid (f (Type n))) => Position -> Annot Range n -> Const (f (Type n)) b
findRange pos =
  let
    isIn (Range rs re) =
      rs <= pos && re >= pos
  in
    (\(Annot r typ _) -> if isIn r then Const (pure typ) else mempty)


-- | Find the type of the subexpression at the given position. Assumes that the
--   input expression is well-typed. Also returns the Src descriptor containing
--   that subexpression if possible.
typeAt :: Position -> ResolvedModule Range Variable -> Maybe (Query.Type Variable)
typeAt pos module_ = do
  let
    wrap (ResolvedFunction a _ s _) = Annot a (schemeType s) []
    exprs2 =               (findRange pos) `traverse` (wrap <$> resolvedEntries module_)
    exprs0 = traverseAnnot (findRange pos) `traverse` (resolvedOutputs module_)
    exprs1 = traverseAnnot (findRange pos) `traverse` (functionDefinition <$> resolvedEntries module_)
    ranged = void exprs0 <> void exprs1 <> void exprs2

  getFirst $
    getConst ranged


sendHover :: State -> JsonRpcId -> Maybe (Type Variable) -> IO ()
sendHover state jrid mType = do
  lspLog  state "* Sending Hover info"

  lspSend state $ object
    [ "id" .= jrid
    , "result" .= object [
        "contents" .= fmap splat mType
      ]
    ]

  where
    splat s = object [
        "language" .= t "icicle"
      , "value"    .= show (pretty s)
      ]

sendNothing :: State -> JsonRpcId -> IO ()
sendNothing state jrid = do
  lspLog  state "* Sending no hover info, we couldn't find the module."
  lspSend state $ object
    [ "id"     .= jrid
    , "result" .= Aeson.Null
    ]

t :: Text -> Text
t = id
