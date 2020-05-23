{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PatternGuards       #-}
module Icicle.LSP.State where

import           Data.Map       (Map)
import           Data.String    (String)
import           Data.IORef
import qualified System.Exit    as System
import qualified System.IO      as System
import           System.IO      (IO, FilePath)

import qualified Icicle.Dictionary as Dictionary
import qualified Icicle.Source.Query as Query

import P

-- | Language server plugin state.
data State
        = State
        { statePhase            :: Phase
        , stateLogDebug         :: Maybe (FilePath, System.Handle)

          -- | Checked core files.
        , stateCoreChecked      :: IORef (Map Query.ModuleName [Dictionary.DictionaryFunction]) }


-- | Phase of the LSP server protocol.
data Phase
        -- | We have just started up and have not yet initialized with the client.
        = PhaseStartup

        -- | Initialization with the client failed.
        | PhaseInitFailed

        -- | We have initialized with the client and are now handling requests.
        | PhaseInitialized
        deriving (Eq, Show)


-- | Append a messgage to the server side log file,  if we have one.
lspLog :: State -> String -> IO ()
lspLog state str
 | Just (_, h)  <- stateLogDebug state
 = do   System.hPutStr h (str <> "\n")
        System.hFlush h

 | otherwise = return ()


-- | Append a message to the server side log file and exit the process.
lspFail :: State -> String -> IO a
lspFail state str
 = do   lspLog state str
        System.die str
