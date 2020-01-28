{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Icicle.Dictionary.Demographics (
    demographics
  ) where

import           Icicle.Data (inputid)
import           Icicle.Dictionary.Data
import           Icicle.Common.Type

import qualified Data.Set as Set
import qualified Data.Map as Map

import           P

-- | Example demographics dictionary
-- Hard-coded for now
demographics :: Dictionary
demographics =
  let
    input i e =
      DictionaryInput i e Set.empty unkeyed

    inputs =
      mapOfInputs [
          input [inputid|default:gender|]
            StringT

        , input [inputid|default:age|]
            IntT

        , input [inputid|default:state_of_residence|]
            StringT

        , input [inputid|default:salary|]
            IntT

        , input [inputid|default:injury|] $
            StructT . StructType $ Map.fromList [
                (StructField "location", StringT)
              , (StructField "severity", IntT)
              ]
        ]

    outputs =
      mapOfOutputs []

    functions =
      []
  in
    Dictionary inputs outputs functions
