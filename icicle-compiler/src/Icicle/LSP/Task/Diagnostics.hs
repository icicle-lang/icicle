{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.LSP.Task.Diagnostics where

import           Icicle.Internal.Pretty hiding ((</>))
import           Icicle.LSP.State
import           Icicle.LSP.Interface
import qualified Icicle.Source.Checker                    as Check
import qualified Icicle.Sorbet.Parse as Sorbet
import           Icicle.Sorbet.Position (Position (..))
import qualified Icicle.Source.Transform.Desugar          as Desugar
import           Icicle.Source.Lexer.Token (Variable)
import           Icicle.Source.Query                      as Query
import qualified Icicle.Compiler.Source as Compiler
import qualified Icicle.Dictionary as Dictionary

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Data.IORef
import           Data.Aeson
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Map as Map
import           Data.String (String)
import qualified Data.Vector as Vector
import qualified Data.List.NonEmpty as NonEmpty

import           System.IO
import           System.FilePath

import           P


-- | Compute diagnostics for a source file, and push them to the client.
updateDiagnostics :: State -> Text -> Text -> IO ()
updateDiagnostics state sUri sSource = do
  diagnostics <- runEitherT $
    updateDiagnostics' state sUri sSource

  case diagnostics of
    Right _ -> do
      sendClearDiagnostics state sUri
    Left fu -> do
      lspLog  state ("* Sending Parse Errors")
      sendDiagnostics state sUri (errorVector fu)


-- | Compute diagnostics for a source file
updateDiagnostics' :: State -> Text -> Text -> EitherT (Compiler.ErrorSource Variable) IO ()
updateDiagnostics' state sUri sSource = do
  funs  <- hoistEither $ Compiler.sourceParseF (takeFileName (Text.unpack sUri)) sSource
  let
    imports =
      Query.moduleImports funs

    rootDir =
      fromMaybe (Text.unpack sUri) $
        List.stripPrefix "file://" $
          takeDirectory (Text.unpack sUri)

  rsvd <- traverse (collectOrAdd rootDir state) imports
  let
    env0 =
      join rsvd <> Dictionary.builtinFunctions

  _    <- hoistEither $ Compiler.sourceDesugarF funs
  _    <- hoistEither $ Compiler.sourceCheckF env0 funs
  return ()


collectOrAdd :: FilePath -> State -> Query.ModuleImport Position -> EitherT (Compiler.ErrorSource Variable) IO [Dictionary.DictionaryFunction]
collectOrAdd rootDir state mi = do
  let
    name =
      Query.importName mi

  known <- liftIO (readIORef (stateCoreChecked state))

  case Map.lookup name known of
    Just x  -> return x
    Nothing -> do
      checked <- Compiler.readIcicleModule (Query.importAnn mi) rootDir name
      let
        loaded =
          fromMaybe [] $ fmap Query.resolvedEntries $ Map.lookup name checked

      liftIO $ modifyIORef (stateCoreChecked state) (Map.insert name loaded)
      return loaded



-- | Clear diagnostics for the given file.
--   We do this when we haven't found any problems with it.
sendClearDiagnostics :: State -> Text -> IO ()
sendClearDiagnostics state sUri = do
  sendDiagnostics state sUri Vector.empty


-- | Clear diagnostics for the given file.
--   We do this when we haven't found any problems with it.
sendDiagnostics :: State -> Text -> Array -> IO ()
sendDiagnostics state sUri diags = do
  lspSend state $ object
    [ "method" .= t "textDocument/publishDiagnostics"
    , "params" .= object [ "uri" .= sUri, "diagnostics" .= diags ]]



-- | Expand and pack compiler error into JSON.
errorVector :: Compiler.ErrorSource Variable -> Vector.Vector Value
errorVector err =
  case err of
    Compiler.ErrorSourceParse pe ->
      Vector.fromList (NonEmpty.toList (fmap packError (Sorbet.positionedParseError pe)))
    Compiler.ErrorSourceDesugar de ->
      Vector.singleton (packError ("Desugar", show (pretty err), Desugar.annotOfError de))
    Compiler.ErrorSourceCheck ce ->
      Vector.singleton (packError ("Check", show (pretty err), Check.annotOfError ce))
    Compiler.ErrorSourceModuleError (Query.ModuleNotFound a fp) ->
      Vector.singleton (packError ("Module", "Couldn't find " <> fp, a))
    Compiler.ErrorImpossible ->
      Vector.empty


-- | Expand and pack a lexer error into JSON.
--
--   The errors we get from a lexer will only indicate the first character
--   that was not part of a valid token. In the editor window we prefer to
--   report the error location from that point until the next space character
--   or end of line, so they're easier to read.
--
packError :: (String, String, Position) -> Value
packError (component, sorbetError, pos)
   = object
     [ "range"       .= join packRange pos
     , "severity"    .= i 1
     , "source"      .= component
     , "message"     .= sorbetError
     ]


packRange :: Position -> Position -> Value
packRange start end
  = object
     [ "start"       .= packLocation start
     , "end"         .= packLocation end
     ]


packLocation :: Position -> Value
packLocation (Position _ nLine nCol)
  = object
      [ "line"       .= i (nLine - 1)
      , "character"  .= i (nCol - 1)
      ]


t :: Text -> Text
t = id


i :: Int -> Int
i = id


hoistWith :: Monad m => (a -> c) -> Either a b -> EitherT c m b
hoistWith f = hoistEither . first f
