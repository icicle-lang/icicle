module Icicle.Source.ToCore.Fold (
    convertFold

  , ConvertFoldResult
  , foldKons
  , foldZero
  , mapExtract
  , typeFold
  , typeExtract

  , groupFoldType
  ) where

import qualified Icicle.Common.Type             as T

import qualified Icicle.Core                    as C

import           Icicle.Source.Query
import           Icicle.Source.ToCore.Base
import           Icicle.Source.Type

import           Data.Hashable                  (Hashable)


data ConvertFoldResult n

foldKons    :: ConvertFoldResult n -> C.Exp () n
foldZero    :: ConvertFoldResult n -> C.Exp () n
mapExtract  :: ConvertFoldResult n -> C.Exp () n
typeFold    :: ConvertFoldResult n -> T.ValType
typeExtract :: ConvertFoldResult n -> T.ValType


convertFold
        :: (Hashable n, Eq n)
        => Query (Annot a n) n
        -> ConvertM a n (ConvertFoldResult n)


groupFoldType
        :: (Type n -> ConvertM a n T.ValType)
        -> a
        -> Exp (Annot a n) n
        -> ConvertM a n (T.ValType, T.ValType)