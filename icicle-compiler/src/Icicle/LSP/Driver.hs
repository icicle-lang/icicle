{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Icicle.LSP.Driver (
    runLSP
  ) where

import           Control.Exception

import           Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.IORef
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import           Icicle.LSP.Interface
import           Icicle.LSP.Protocol
import           Icicle.LSP.State
import           Icicle.LSP.Task.Diagnostics as Task
import           Icicle.LSP.Task.Hover as Task

import           System.FilePath
import           System.IO as System
import qualified System.Posix.Process as Process

import qualified Text.Show.Pretty as T

import           P

---------------------------------------------------------------------------------------------------

-- | Become a language server plugin.
--
--   * We listen to requests on stdin and send responses to stdout.
--   * We take an optional path for server side logging.
--   * If the server process crashes then try to write the reason to the debug log.
runLSP :: Maybe FilePath -> IO ()
runLSP mFileLog = do
  state <- lspBegin mFileLog
  lspLoop state `onException'` (lspLog state . displayException)


---------------------------------------------------------------------------------------------------

-- | The main event loop for the language server.
--
--   We do a blocking read of stdin to get a request,
--   then dispatch it to the appropriat
lspLoop :: State -> IO ()
lspLoop state =
  case statePhase state of
    PhaseStartup ->
      do
        msg <- lspRead state
        lspStartup state msg

    PhaseInitialized ->
      do
        msg <- lspRead state
        lspInitialized state msg

    PhaseInitFailed -> do
      lspLog state "Initialisation failed"

---------------------------------------------------------------------------------------------------

-- | Begin the language server plugin.
--   We open our local file and initialize the state.
lspBegin :: Maybe FilePath -> IO State
lspBegin mFileLog =
  do
    pid <- Process.getProcessID
    -- Create a new file for the debug log, if we were asked for one.
    mLogDebug <-
      for mFileLog $ \fileLog -> do
        let fileLogPid = fileLog <> "." <> show pid
        hLogDebug <- System.openFile fileLogPid System.WriteMode
        return (fileLogPid, hLogDebug)

    -- The type checked modules are stored here, when we have them
    refCoreChecked <- newIORef Map.empty
    -- The complete state.
    let
      state =
        State {
            stateLogDebug = mLogDebug
          , statePhase = PhaseStartup
          , stateCoreChecked = refCoreChecked
          }
    lspLog state "* Icicle language server starting up"
    return state

---------------------------------------------------------------------------------------------------

-- | Handle startup phase where we wait for the
--   initializatino request sent from the client.
lspStartup :: State -> Request Value -> IO ()
lspStartup state req
  -- Client sends us 'inititialize' with the set of its capabilities.
  -- We reply with our own capabilities.
  | "initialize" <- reqMethod req
  = do
      lspLog state "* Initialize"
      -- It's highly unlikely these aren't supported.
      lspSend state $
        object
          [ "id" .= reqId req,
            "result"
              .= object
                [ "capabilities"
                    .= object
                      [ "textDocumentSync"
                          .= object
                            [ "openClose" .= True,    -- send us open/close notif.
                              "change" .= (1 :: Int), -- send us full file changes.
                              "save" .= True          -- send us save notification.
                            ]
                      , "hoverProvider"
                          .= True
                      ]
                ]
          ]

      lspLoop state

  -- Client sends us 'initialized' if it it is happy with the
  -- capabilities that we sent.
  | "initialized" <- reqMethod req =
    do
      lspLog state "* Initialized"
      lspLoop state { statePhase = PhaseInitialized }

  -- Something went wrong.
  | otherwise =
    do
      lspLog state "* Initialization received unexpected message."
      lspLoop state


---------------------------------------------------------------------------------------------------
-- | Main event handler of the server.
--
--   Once initialized we receive the main requests and update our state.
--
--   This is weakly typed intentionally here. We're just bashing on json
--   explicitly.
lspInitialized :: State -> Request Value -> IO ()
lspInitialized state req

  --   On startup VSCode sends us a didChangeConfiguration,
  --   but we don't have any settings define, so the payload is empty.
  --   Just drop it on the floor.
  | "workspace/didChangeConfiguration" <- reqMethod req
  , Just (Object jParams)              <- reqParams req
  = do
         lspLog state "* DidChangeConfiguration (icicle)"
         lspLog state $ "  jSettings:    " <> show jParams
         lspLoop state

  -- A file was opened.
  | "textDocument/didOpen"    <- reqMethod req
  , Just (Object jParams)     <- reqParams req
  , Just (Object jDoc)        <- KeyMap.lookup "textDocument" jParams
  , Just (String sUri)        <- KeyMap.lookup "uri" jDoc
  , Just (String sLanguageId) <- KeyMap.lookup "languageId" jDoc
  , Just (Number iVersion)    <- KeyMap.lookup "version" jDoc
  , Just (String sText)       <- KeyMap.lookup "text" jDoc
  = do
        lspLog state "* DidOpen"
        lspLog state $ "  sUri:         " <> show sUri
        lspLog state $ "  sLanguageId:  " <> show sLanguageId
        lspLog state $ "  iVersion:     " <> show iVersion
        lspLog state $ "  sText:        " <> show sText

        Task.updateDiagnostics state sUri sText
        lspLoop state

  -- A file was changed.
  | "textDocument/didChange"  <- reqMethod req
  , Just (Object jParams)     <- reqParams req
  , Just (Object jDoc)        <- KeyMap.lookup "textDocument" jParams
  , Just (String sUri)        <- KeyMap.lookup "uri" jDoc
  , Just (Number iVersion)    <- KeyMap.lookup "version" jDoc
  , Just (Array jChangeArr)   <- KeyMap.lookup "contentChanges" jParams
  , [Object jChange]          <- Vector.toList jChangeArr
  , Just (String sText)       <- KeyMap.lookup "text" jChange
  = do
        lspLog state "* DidChange"
        lspLog state $ "  sUri:         " <> show sUri
        lspLog state $ "  iVersion:     " <> show iVersion
        lspLog state $ "  sText:        " <> show sText
        Task.updateDiagnostics state sUri sText
        lspLoop state


  -- A file was closed.
  | "textDocument/didClose" <- reqMethod req
  , Just (Object jParams)   <- reqParams req
  , Just (Object jDoc)      <- KeyMap.lookup "textDocument" jParams
  , Just (String sUri)      <- KeyMap.lookup "uri" jDoc
  = do
        lspLog state "* DidClose"
        lspLog state $ "  sUri:         " <> show sUri

        -- Once the file is closed, clear any errors that it might still have
        -- from the IDE and remove it from the cache.
        Task.closeDiagnostics state sUri
        lspLoop state

  -- A file was saved.
  -- Update the cached version so different modules can pick up
  -- the new definitions.
  | "textDocument/didSave"  <- reqMethod req
  , Just (Object jParams)   <- reqParams req
  , Just (Object jDoc)      <- KeyMap.lookup "textDocument" jParams
  , Just (String sUri)      <- KeyMap.lookup "uri" jDoc
  , Just (Number iVersion)  <- KeyMap.lookup "version" jDoc
  = do  lspLog state "* DidSave"
        lspLog state $ "  sUri:         " <> show sUri
        lspLog state $ "  iVersion:     " <> show iVersion
        Task.saveDiagnostics state sUri
        lspLoop state

  -- A hover request.
  -- Keeping it simple here, pull out what we need, and the
  -- Task will post either a response or nothing
  | "textDocument/hover"    <- reqMethod req
  , Just (Object jParams)   <- reqParams req
  , Just jrid               <- reqId     req
  , Just (Object jDoc)      <- KeyMap.lookup "textDocument" jParams
  , Just (String sUri)      <- KeyMap.lookup "uri" jDoc
  , Just (Object jPos)      <- KeyMap.lookup "position" jParams
  , Just (Number pLine)     <- KeyMap.lookup "line" jPos
  , Just (Number pChar)     <- KeyMap.lookup "character" jPos
  = do  lspLog state "* Hover"
        Task.hover state sUri jrid (round pLine) (round pChar)
        lspLoop state

  -- Some other request that we don't handle.
  | otherwise
  = do
        lspLog  state "* Request"
        lspLog  state (T.ppShow req)
        lspLoop state


-- | Like 'finally', but only performs the final action if there was an
-- exception raised by the computation.
onException' :: IO a -> (SomeException -> IO b) -> IO a
onException' io what =
  io `catch` (\e -> what e *> throwIO e)
