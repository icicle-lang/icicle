{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.LSP.Task.Diagnostics where

import           Icicle.Internal.Pretty
import           Icicle.LSP.State
import           Icicle.LSP.Interface
import qualified Icicle.Sorbet.Parse as Sorbet
import           Icicle.Sorbet.Position (Position (..), fromParsec)
import qualified Icicle.Source.Checker as Check
import           Icicle.Source.Query (reannot)
import           Icicle.Source.Lexer.Token (Variable)
import           Icicle.Compiler.Source (FunEnvT, Funs, freshNamer)
import qualified Icicle.Common.Fresh as Fresh
import qualified Icicle.Dictionary as Dictionary

import           Control.Monad.Trans.Either

import           Data.Aeson
import           Data.Hashable (Hashable)
import qualified Data.Text as Text
import           Data.String (String, IsString)
import qualified Data.Vector as Vector
import qualified Data.List.NonEmpty as NonEmpty

import           System.IO
import           System.FilePath

import           P


-- | Compute diagnostics for a source file, and push them to the client.
updateDiagnostics :: State -> Text -> Text -> IO ()
updateDiagnostics state sUri sSource = do
  let
    parseResult =
      Sorbet.sorbetFunctions (takeFileName (Text.unpack sUri)) sSource

  case parseResult of
    Right funs -> do
      updateCheckDiagnostics state sUri funs
    Left fu -> do
      lspLog  state ("* Sending Parse Errors")
      sendDiagnostics state sUri (Vector.fromList (NonEmpty.toList (fmap packError (Sorbet.positionedParseError fu))))


updateCheckDiagnostics
  :: State
  -> Text
  -> Funs Position Variable
  -> IO ()
updateCheckDiagnostics state sUri funs = do
  let
    checkResult =
      sourceCheckF (reannot fromParsec <$> Dictionary.dictionaryFunctions Dictionary.emptyDictionary) funs

  case checkResult of
    Right _ -> do
      sendClearDiagnostics state sUri
    Left fu -> do
      lspLog  state ("* Sending Check Errors")
      sendDiagnostics state sUri (Vector.singleton (packError ("Checker", show (pretty fu), Check.annotOfError fu)))


sourceCheckF
  :: (Eq v, IsString v, Hashable v, Pretty v)
  => FunEnvT Position v
  -> Funs    Position v
  -> Either  (Check.CheckError Position v) (FunEnvT Position v)
sourceCheckF env parsedImport
 = second fst
 $ first fst
 $ snd
 $ flip Fresh.runFresh (freshNamer "check")
 $ runEitherT
 $ Check.checkFs env
 $ parsedImport


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
