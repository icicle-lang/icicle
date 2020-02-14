{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.LSP.Protocol.Request where

import Icicle.LSP.Protocol.Base

import Data.Aeson
import Data.String

import P

-- | Client request.
data Request a
  = Request
  { reqId         :: JsonRpcId
  , reqMethod     :: String
  , reqParams     :: Maybe a }

  | Notification
  { reqMethod     :: String
  , reqParams     :: Maybe a }
  deriving Show

instance FromJSON a => FromJSON (Request a) where
  parseJSON (Object v) =
    let
      parseReq = do
        Request
          <$> v .:  "id"
          <*> v .:  "method"
          <*> v .:? "params"

      parseNotification =
        Notification
          <$> v .:  "method"
          <*> v .:? "params"

    in
      parseReq <|> parseNotification

  parseJSON _ =
    fail "Couldn't parse request"