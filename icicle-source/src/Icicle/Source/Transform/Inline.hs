{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Transform.Inline (
    inlineTransform
  , inlineQT
  , inlineQ
  , inlineX
  , InlineOption (..)
  , defaultInline
  ) where

import Icicle.Source.Query
import Icicle.Source.Type
import Icicle.Source.Transform.Base

import Icicle.Common.Base
import Icicle.Common.Fresh

import P

import              Data.List (zip)
import qualified    Data.Map as Map
import              Data.Hashable (Hashable)

type FunMap a n = Map.Map (Name n) (Exp (Annot a n) n)

data InlineOption
  = InlineUsingSubst

defaultInline :: InlineOption
defaultInline = InlineUsingSubst

inlineTransform
        :: (Hashable n, Eq n)
        => InlineOption
        -> FunMap a n
        -> Transform (Fresh n) () (Annot a n) n
inlineTransform _ funs
 = Transform
 { transformExp     = tranx
 , transformPat     = tranp
 , transformContext = tranc
 , transformState   = ()
 }
 where
  tranx _ x
   | Var _ n   <- x
   , Just fun  <- Map.lookup n funs
   = return ((), fun)

   | otherwise
   = return ((), x)

  tranp _ p
   = return ((), p)

  tranc _ c
   = return ((), c)

inlineQT :: (Hashable n, Eq n)
        => InlineOption
        -> Map.Map (Name n) (Exp (Annot a n) n)
        -> QueryTop (Annot a n) n
        -> Fresh n (QueryTop (Annot a n) n)
inlineQT opt funs qt
 = simplifyNestedQT <$> transformQT (inlineTransform opt funs) qt


inlineQ :: (Hashable n, Eq n)
        => InlineOption
        -> Map.Map (Name n) (Exp (Annot a n) n)
        -> Query (Annot a n) n
        -> Fresh n (Query (Annot a n) n)
inlineQ opt funs
 = transformQ (inlineTransform opt funs)

inlineX :: (Hashable n, Eq n)
        => InlineOption
        -> Map.Map (Name n) (Exp (Annot a n) n)
        -> Exp (Annot a n) n
        -> Fresh n (Exp (Annot a n) n)
inlineX opt funs
 = transformX (inlineTransform opt funs)

