-- | Beta and let reduction for simple values
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Simp.Beta (
      beta
    , betaToLets
    , isSimpleValue
    ) where

import Control.Lens.Plated (transform)

import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Compounds

import P

import Data.Hashable (Hashable)

-- | Beta and let reduction
beta :: (Hashable n, Eq n) => Exp a n p -> Exp a n p
beta
 = transform go
 where
  go xx
   = case xx of
      XApp _ (XLam _ n _ (XVar a' n')) q
       -> if   n == n'
          then q
          else XVar a' n'

      XApp _ (XLam _ n _ x) q
       | isSimpleValue q
       , Just x' <- substMaybe n q x
       -> x'

      XLet _ n v (XVar a' n')
       -> if   n == n'
          then v
          else XVar a' n'

      XLet _ n v x
       | isSimpleValue v
       , Just x' <- substMaybe n v x
       -> x'

      XLet {}         -> xx
      XApp {}         -> xx
      XLam {}         -> xx
      XValue{}        -> xx
      XVar{}          -> xx
      XPrim{}         -> xx

-- | Total beta: always convert (\n. f) x to  (let n = x in f)
betaToLets :: a -> Exp a n p -> Exp a n p
betaToLets a_let toplevel
 = go toplevel
 where
  go xx
   = case xx of
      XApp _ _ _
       | (f, x:xs)    <- takeApps xx
       , XLam _ n _ b <- f
       -> XLet a_let n (go x) (go $ makeApps a_let b xs)

      XApp a p q
       -> XApp a (go p) (go q)

      XLam a n t x
       -> XLam a n t $ go x

      XLet a n v x
       -> XLet a n (go v) (go x)

      XValue{}        -> xx
      XVar{}          -> xx
      XPrim{}         -> xx



-- | Check if expression is just a primitive or a variable.
isSimpleValue :: Exp a n p -> Bool
isSimpleValue xx
 = case xx of
    XPrim{} -> True
    XVar{}  -> True
    XValue{}-> True
    XLam{}  -> True
    _       -> False
