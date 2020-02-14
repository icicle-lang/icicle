{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.LSP.Protocol.Initialize where

import           Data.Aeson
import           P

-- | Request from the client to initialize the server.
data InitializeParams
  = InitializeParams
  { ipProcessId                   :: Maybe Integer
  , ipRootUri                     :: Maybe Text
  , ipInitializationOptions       :: Maybe Value
  , ipCapabilities                :: [(Text, Value)]
  , ipTrace                       :: Maybe Text
  , ipWorkspaceFolders            :: Maybe Value
  } deriving Show
