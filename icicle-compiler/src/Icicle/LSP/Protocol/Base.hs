
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.LSP.Protocol.Base (
  -- * JsonRpcId
    JsonRpcId (..)
  ) where

import           Data.Aeson          as Aeson
import qualified Data.Scientific     as Scientific

import           P

---------------------------------------------------------------------------------------------------
data JsonRpcId
  = JsonRpcIdInt    Int
  | JsonRpcIdString Text
  | JsonRpcIdNull
  deriving Show


instance ToJSON JsonRpcId where
  toJSON = \case
    JsonRpcIdInt i    -> toJSON i
    JsonRpcIdString s -> toJSON s
    JsonRpcIdNull     -> Aeson.Null


instance FromJSON JsonRpcId where
  parseJSON js
    | Number s <- js
    , Just i   <- Scientific.toBoundedInteger s
    = return $ JsonRpcIdInt i

    | String s <- js
    = return $ JsonRpcIdString s

    | Null     <- js
    = return $ JsonRpcIdNull

    | otherwise
    = fail "Coudn't parser JsonRpcId"
