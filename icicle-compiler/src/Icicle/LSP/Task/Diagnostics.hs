{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse   #-}
module Icicle.LSP.Task.Diagnostics (
    updateDiagnostics
  , saveDiagnostics
  , closeDiagnostics
  , sendClearDiagnostics

  , updateDiagnostics'
  ) where

import           Icicle.Internal.Pretty hiding ((</>))
import           Icicle.LSP.State
import           Icicle.LSP.Interface
import qualified Icicle.Source.Checker                    as Check
import qualified Icicle.Sorbet.Parse as Sorbet
import           Icicle.Sorbet.Position (Range (..), Position (..))
import qualified Icicle.Source.Transform.Desugar          as Desugar
import           Icicle.Source.Lexer.Token (Variable)
import           Icicle.Source.Query                      as Query
import qualified Icicle.Compiler.Source as Compiler
import qualified Icicle.Dictionary as Dictionary

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Data.IORef
import           Data.Aeson
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Map as Map
import           Data.String (String)
import qualified Data.Vector as Vector
import qualified Data.List.NonEmpty as NonEmpty

import           System.IO
import           System.FilePath

import           P


-- | Compute diagnostics for a source file, and push them to the client.
--
--   This is called when the document is unsaved and under test. So we're
--   not looking at the original file at this point.
updateDiagnostics :: State -> Text -> Text -> IO ()
updateDiagnostics state sUri sSource = do
  diagnostics <- runEitherT $
    updateDiagnostics' state sUri sSource

  let
    localPath =
      fromMaybe (Text.unpack sUri) $
        List.stripPrefix "file://" $
          Text.unpack sUri

  case diagnostics of
    Right module_ -> do
      modifyIORef (stateCoreChecked state) (Map.insert localPath module_)
      sendClearDiagnostics state sUri

    Left fu -> do
      lspLog  state ("* Sending Parse Errors")
      modifyIORef (stateCoreChecked state) (Map.delete localPath)
      sendDiagnostics state sUri (errorVector fu)


-- | Compute diagnostics for an in memory module
updateDiagnostics' :: State -> Text -> Text -> EitherT (Compiler.ErrorSource Variable) IO (ResolvedModule Range Variable)
updateDiagnostics' state sUri sSource = do
  funs    <- hoistEither $ Compiler.sourceParseF (takeFileName (Text.unpack sUri)) sSource
  prelude <- hoistEither $ Compiler.loadedPrelude >>= Compiler.sourceCheckF Check.defaultCheckOptions Dictionary.builtinFeatures

  let
    imports =
      Query.moduleImports funs

    modName =
      Query.moduleName funs

    rootDir =
      fromMaybe (Text.unpack sUri) $
        List.stripPrefix "file://" $
          takeDirectory (Text.unpack sUri)

    implicitPrelude =
      if modName == Query.ModuleName "Prelude" then
        []
      else
        [prelude]

  rsvd <- traverse (collectOrAdd rootDir state) imports
  let
    env0 =
      Dictionary.featureMapOfModules (rsvd <> implicitPrelude)

  _     <- hoistEither $ Compiler.sourceDesugarF funs
  funs' <- hoistEither $ Compiler.sourceCheckF Check.defaultCheckOptions env0 funs
  return funs'


-- | Compute diagnostics for a source file, and push them to the client.
--
--   This is called when the document is being saved. So we want to look
--   at the file on disk, and make sure our in-memory cache of resolved
--   modules is updated with the results.
saveDiagnostics :: State -> Text -> IO ()
saveDiagnostics state sUri = do
  diagnostics <- runEitherT $
    saveDiagnostics' state sUri

  case diagnostics of
    Right _ -> do
      sendClearDiagnostics state sUri
    Left fu -> do
      lspLog  state ("* Sending Parse Errors")
      sendDiagnostics state sUri (errorVector fu)

-- | Compute diagnostics for a source file, and push them to the client.
--
--   This is called when the document is being saved. So we want to look
--   at the file on disk, and make sure our in-memory cache of resolved
--   modules is updated with the results.
closeDiagnostics :: State -> Text -> IO ()
closeDiagnostics state sUri = do
  let
    localPath =
      fromMaybe (Text.unpack sUri) $
        List.stripPrefix "file://" $
          Text.unpack sUri

  liftIO $ modifyIORef (stateCoreChecked state) (Map.delete localPath)
  sendClearDiagnostics state sUri

-- | Compute diagnostics for an on disk module
saveDiagnostics' :: State -> Text -> EitherT (Compiler.ErrorSource Variable) IO ()
saveDiagnostics' state sUri = do
  let
    localPath =
      fromMaybe (Text.unpack sUri) $
        List.stripPrefix "file://" $
          Text.unpack sUri

    rootDir =
      takeDirectory localPath

    sourceName =
      takeFileName localPath

  input <- Text.decodeUtf8 <$> liftIO (ByteString.readFile localPath)
  modul <- Compiler.readIcicleLibrary Check.defaultCheckOptions rootDir sourceName input

  liftIO (lspLog  state ("* Cache rewrite for " <> localPath))
  liftIO $ modifyIORef (stateCoreChecked state) (Map.insert localPath modul)


-- Check our in memory cache of checked modules
-- If the module hasn't been loaded yet, then
-- add it to our cache.
-- collectOrAdd :: FilePath -> State -> Query.ModuleImport Range -> EitherT (Compiler.ErrorSource Variable) IO (Query.Features a Variable k)
collectOrAdd :: FilePath -> State -> ModuleImport Range -> EitherT (Compiler.ErrorSource Variable) IO (ResolvedModule Range Variable)
collectOrAdd rootDir state mi = do
  let
    name =
      Query.importName mi

  if name == Query.ModuleName "Prelude" then
    hoistEither $ Compiler.loadedPrelude >>= Compiler.sourceCheckF Check.defaultCheckOptions Dictionary.builtinFeatures

  else do
    expected_location <-
      firstEitherT Compiler.ErrorSourceModuleError $ Query.getModuleFileName rootDir mi

    known <- liftIO (readIORef (stateCoreChecked state))
    case Map.lookup expected_location known of
      Just x  -> do
        liftIO (lspLog  state ("* Cache hit for " <> expected_location))
        return x
      Nothing -> do
        liftIO (lspLog  state ("* Cache miss for " <> expected_location))
        checked <- Compiler.readIcicleModule Check.defaultCheckOptions (Query.importAnn mi) rootDir name
        loaded  <-
          maybe (left Compiler.ErrorImpossible) pure $
            Map.lookup name checked

        liftIO (lspLog  state ("* Cache write for " <> expected_location))
        liftIO $ modifyIORef (stateCoreChecked state) (Map.insert expected_location loaded)
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


pointRange :: Position -> Range
pointRange p = Range p p

-- | Expand and pack compiler error into JSON.
errorVector :: Compiler.ErrorSource Variable -> Vector.Vector Value
errorVector err =
  case err of
    Compiler.ErrorSourceParse pe ->
      Vector.fromList (NonEmpty.toList (fmap (packError . fmap pointRange) (Sorbet.positionedParseError pe)))
    Compiler.ErrorSourceDesugar de ->
      Vector.singleton (packError ("Desugar", show (pretty err), Desugar.annotOfError de))
    Compiler.ErrorSourceCheck ce ->
      Vector.singleton (packError ("Check", show (pretty err), Check.annotOfError ce))
    Compiler.ErrorSourceModuleError _ ->
      Vector.singleton (packError ("Module", show (pretty err), pointRange (Position "<>" 1 1)))
    Compiler.ErrorImpossible ->
      Vector.empty


-- | Expand and pack a lexer error into JSON.
--
--   The errors we get from a lexer will only indicate the first character
--   that was not part of a valid token. In the editor window we prefer to
--   report the error location from that point until the next space character
--   or end of line, so they're easier to read.
--
packError :: (String, String, Range) -> Value
packError (component, sorbetError, pos)
   = object
     [ "range"       .= packRange pos
     , "severity"    .= i 1
     , "source"      .= component
     , "message"     .= sorbetError
     ]


packRange :: Range -> Value
packRange (Range start end)
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


