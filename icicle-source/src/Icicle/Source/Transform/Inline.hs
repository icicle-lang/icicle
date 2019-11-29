{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Icicle.Source.Transform.SubstX

import Icicle.Common.Base
import Icicle.Common.Fresh

import P

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
        -> Transform (Fresh n) (FunMap a n) (Annot a n) n
inlineTransform _ funs0
 = Transform
 { transformExp     = tranx
 , transformPat     = tranp
 , transformContext = tranc
 , transformState   = funs0
 }
 where
  tranx funs x
   | (Var _ n, args)   <- takeApps x
   , Just fun          <- Map.lookup n funs
   , (arguments, fapp) <- takeLams fun
   , length arguments  >= length args
   = do let (xx, dgl)   = args `zipDangle` arguments
            sub         = Map.fromList
                        $ fmap (\(b, (_,a)) -> (a,b))
                        $ xx

        inlined  <- substX sub fapp
        return (funs, makeLams dgl inlined)

   | otherwise
   = return (funs, x)

  tranp funs p
   = return (funs, p)

  tranc funs c
   | Let ann (PatVariable n) x <- c
   , Lam {} <- x
   = do (funs', x') <- tranx funs x
        let inlining = Map.insert n x' funs'
        return (inlining, Let ann PatDefault x)

   | Let _ pat _ <- c
   = return (foldl (flip Map.delete) funs (snd $ allvarsP pat), c)
   | LetFold _ (Fold pat _ _ _) <- c
   = return (foldl (flip Map.delete) funs (snd $ allvarsP pat), c)
   | GroupFold _ k v _ <- c
   = return (foldl (flip Map.delete) funs (snd $ allvarsP (PatCon ConTuple [k, v])), c)
   | otherwise
   = return (funs, c)

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

zipDangle :: [a] -> [b] -> ([(a, b)], [b])
zipDangle    _      []  = ([],[])
zipDangle    []     bs  = ([],bs)
zipDangle (a:as) (b:bs) = ((a,b) : cs , sv)
  where (cs, sv) = zipDangle as bs
