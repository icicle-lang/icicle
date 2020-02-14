{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
module Icicle.LSP.Interface where

import Icicle.LSP.Protocol.Request
import Icicle.LSP.State
import qualified System.IO              as S
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Text.Read              as T

import Data.Aeson

import System.IO (IO)
import P

-- | Read a JsonRPC message from stdin.
lspRead :: forall a. (Show a, FromJSON a)
        => State -> IO (Request a)
lspRead state
 = do   lspLog state "* Waiting for message"
        txContentLength <- T.hGetLine S.stdin

        txLength1
         <- case T.stripPrefix "Content-Length: " txContentLength of
                Just tx -> return tx
                Nothing -> lspFail state "Invalid JsonRPC header from client: no Content-Length"

        txLength
         <- case T.stripSuffix "\r" txLength1 of
                Just tx -> return tx
                Nothing -> lspFail state "Invalid JsonRPC header from client: no CR after length"

        lenChunk
         <- case T.readMaybe (T.unpack txLength) of
                Just n  -> return n
                Nothing -> lspFail state "Invalid JsonRPC header from client: bad length"

        sCR     <- T.hGetLine S.stdin
        (case sCR of
                "\r"    -> return ()
                _       -> lspFail state "Invalid JsonRPC header from client: no CR")

        txChunk  <- lspReadChunk state lenChunk

        case eitherDecode txChunk of
              Left str ->
                  do lspLog state $ "  error: " <> show str
                     lspRead state

              Right js ->
                 do  lspLog state $ show txChunk
                     return js


-- | Read a chunk of the given size from stdin.
---
--   Careful: the Content-Length needs to be the length of the utf8
--   encoded bytestring, not the number of unicode characters.
--   If this is wrong then the stream will get out of sync.
--
lspReadChunk :: State -> Int -> IO LBS.ByteString
lspReadChunk _state nChunk
 = LBS.hGet S.stdin nChunk


-- | Send a JSON value via JsonRPC to the client.
--   We print it to Stdout with the content-length headers.
---
--   Careful: the Content-Length needs to be the length of the utf8
--   encoded bytestring, not the number of unicode characters.
--   If this is wrong then the stream will get out of sync.
--
lspSend :: State -> Value -> IO ()
lspSend _state js
 = do   let str = encode js
        let bs  = str
        S.putStr $ "Content-Length: " <> show (LBS.length bs) <> "\r\n"
        S.putStr $ "\r\n"
        LBS.hPut S.stdout bs
        S.hFlush S.stdout

