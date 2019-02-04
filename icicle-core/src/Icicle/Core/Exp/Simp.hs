{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE LambdaCase        #-}
module Icicle.Core.Exp.Simp
     ( simp
     , simpX
     , simpP
     , deadX
     ) where

import           Icicle.Common.Value
import           Icicle.Common.Base
import           Icicle.Common.Exp              hiding (simp)
import           Icicle.Common.Exp.Simp.ANormal
import qualified Icicle.Common.Exp.Simp.Beta    as B
import qualified Icicle.Common.Exp.Prim.Minimal as Min
import           Icicle.Common.Fresh
import           Icicle.Common.Type
import           Icicle.Common.FixT
import qualified Icicle.Core.Exp                as C
import           Icicle.Core.Exp.Prim
import qualified Icicle.Core.Eval.Exp           as CE

import           P

import           Control.Monad.Trans.Class      (lift)

import qualified Data.Set                       as Set
import           Data.Hashable                  (Hashable)

-- | Core Simplifier:
simp :: (Hashable n, Eq n) => a -> C.Exp a n -> Fresh n (C.Exp a n)
simp a_fresh = anormal a_fresh <=< fmap deadX . fixpoint (allSimp a_fresh)

allSimp :: (Hashable n, Eq n)
        => a -> C.Exp a n -> FixT (Fresh n) (C.Exp a n)
allSimp a_fresh
  =   simpX a_fresh
  >=> caseOfCase a_fresh
  >=> caseConstants a_fresh
  >=> unshuffleLets a_fresh

-- | Core Simplifier:
--   * a normal
--   * beta reduction
--   * constant folding for some primitives
simpX :: (Monad m, Hashable n, Eq n)
      => a -> C.Exp a n -> FixT m (C.Exp a n)
simpX a_fresh = go . B.beta
  where
    -- * constant folding for some primitives
    go xx = case xx of
      XApp a p q
       -> do p' <- go p
             q' <- go q
             let x' = XApp a p' q'
             case takePrimApps x' of
               Just (prim, as)
                 | Just args <- mapM takeValue as
                 -> case simpP a_fresh prim args of
                      Just x''
                        -> progress x''
                      _ -> return x'
               _ -> return x'

      XLam a n t x1
        -> XLam a n t <$> go x1

      XLet a n p q
        -> XLet a n <$> go p <*> go q

      XVar{}   -> return xx
      XPrim{}  -> return xx
      XValue{} -> return xx

-- | Primitive Simplifier
simpP :: (Hashable n, Eq n) => a -> Prim -> [Value a n Prim] -> Maybe (C.Exp a n)
simpP a_fresh p vs
 | length (functionArguments $ C.typeOfPrim p) == length vs
 = case CE.evalPrim p vs of
    Right (VBase b)
     -> Just
      $ XValue a_fresh (functionReturns $ C.typeOfPrim p) b
    -- TODO: we could actually pull the
    -- heap out as let bindings, and so on..
    Right VFun{}
     -> Nothing
    Left _
     -> Nothing
 | otherwise
 = Nothing


-- | Dead binding removal
deadX :: Eq n => C.Exp a n -> C.Exp a n
deadX = fst . go
  where
    go xx = case xx of
      XApp a p q
        -> let (px, pf) = go p
               (qx, qf) = go q
           in  (XApp a px qx, Set.union pf qf)

      XLam a n t x1
        -> let (x1', fs) = go x1
           in  (XLam a n t x1', Set.delete n fs)

      XLet a n x1 x2
        -> let (x2', f2) = go x2
               (x1', f1) = go x1
           in  if   n `Set.member` f2
               then (XLet a n x1' x2', Set.union f1 f2)
               else (             x2',              f2)

      b@(XVar _ n) -> (b, Set.singleton n)
      b@(XPrim{})  -> (b, Set.empty)
      b@(XValue{}) -> (b, Set.empty)

-- | Case of Irrefutable Case.
caseOfCase :: (Hashable n, Eq n)
           => a -> C.Exp a n -> FixT (Fresh n) (C.Exp a n)
caseOfCase a_fresh = go
  where
    go xx = goApp xx >>= \x' -> case takePrimApps x' of
      Just (primitive, as)
        | PrimFold _ ret'typ <- primitive
        , [XLam _ l'n _ l'exp, XLam _ r'n _ r'exp, scrut] <- as
        -> case takePrimApps scrut of
            Just (primitive', ass)
              | PrimFold fld _ <- primitive'
              , [XLam _ i'l'n i'l'typ i'l'exp, XLam _ i'r'n i'r'typ i'r'exp, scrutinee] <- ass
              , Just (l'case, r'case) <- (,) <$> takeIrrefutable i'l'exp <*> takeIrrefutable i'r'exp
              -> do
                   n'l <- lift fresh
                   n'r <- lift fresh

                   let
                    renameLeftWorker m
                      | m == i'l'n = n'l
                      | otherwise  = m
                    renameLeft
                      = renameExp renameLeftWorker

                    renameRightWorker m
                      | m == i'r'n = n'r
                      | otherwise  = m
                    renameRight
                      = renameExp renameRightWorker

                    n'l'lam
                     = xlam n'l i'l'typ
                     $ xlet l'n (either renameLeft renameLeft l'case) (either (const l'exp) (const r'exp) l'case)

                    n'r'lam
                     = xlam n'r i'r'typ
                     $ xlet r'n (either renameRight renameRight r'case) (either (const l'exp) (const r'exp) r'case)

                   progress
                     $ xprim (PrimFold fld ret'typ)
                      `xapp` n'l'lam
                      `xapp` n'r'lam
                      `xapp` scrutinee

            _ -> return x'
      _ -> return x'


    xapp = XApp a_fresh
    xprim = XPrim a_fresh
    xlet = XLet a_fresh
    xlam = XLam a_fresh

    goApp xx = case xx of
      XApp _ p q
       -> do p' <- go p
             q' <- go q
             return $ XApp a_fresh p' q'

      XLam a n t x1
        -> XLam a n t <$> go x1

      XLet a n p q
        -> XLet a n <$> go p <*> go q

      XVar{}   -> return xx
      XPrim{}  -> return xx
      XValue{} -> return xx

-- | Simplification when both sides of a fold
--   are wrapped with the same constructor.
--
--   By itself this doesn't do much, but it does
--   permit the case of case optimisation and has
--   no real cost itself.
caseConstants :: (Monad m, Hashable n, Eq n)
              => a -> C.Exp a n -> FixT m (C.Exp a n)
caseConstants a_fresh = go
  where
    go xx = goApp xx >>= \x' -> case takePrimApps x' of
      Just (primitive, as)
        | PrimFold a (SumT ret'l ret'r) <- primitive
        , [XLam _ l'n l'typ l'exp, XLam _ r'n r'typ r'exp, scrut] <- as
        , Just (l'side, r'side) <- (,) <$> takeIrrefutable l'exp <*> takeIrrefutable r'exp
        -> case (l'side, r'side) of
             (Left x, Left y) ->
               progress $
                 xapp (xleft ret'l ret'r)
                  $ xprim (PrimFold a ret'l)
                   `xapp` xlam l'n l'typ x
                   `xapp` xlam r'n r'typ y
                   `xapp` scrut
             (Right x, Right y) ->
               progress $
                 xapp (xright ret'l ret'r)
                  $ xprim (PrimFold a ret'r)
                   `xapp` xlam l'n l'typ x
                   `xapp` xlam r'n r'typ y
                   `xapp` scrut
             _ -> return x'

      _ -> return x'

    xapp = XApp a_fresh
    xprim = XPrim a_fresh
    xlam = XLam a_fresh
    xleft l r = xprim $ PrimMinimal (Min.PrimConst (Min.PrimConstLeft l r))
    xright l r = xprim $ PrimMinimal (Min.PrimConst (Min.PrimConstRight l r))

    goApp xx = case xx of
      XApp _ p q
       -> do p' <- go p
             q' <- go q
             return $ XApp a_fresh p' q'

      XLam a n t x1
        -> XLam a n t <$> go x1

      XLet a n p q
        -> XLet a n <$> go p <*> go q

      XVar{}   -> return xx
      XPrim{}  -> return xx
      XValue{} -> return xx

takeIrrefutable :: Exp a n Prim -> Maybe (Either (Exp a n Prim) (Exp a n Prim))
takeIrrefutable xx = case xx of
  XApp {}
    -> case takePrimApps xx of
        Just (prim, as)
          | PrimMinimal (Min.PrimConst Min.PrimConstRight {}) <- prim
          , [rhs] <- as
          -> Just (Right rhs)
          | PrimMinimal (Min.PrimConst Min.PrimConstLeft {}) <- prim
          , [lhs] <- as
          -> Just (Left lhs)
        _ -> Nothing

  XLam a n t x1
    -> XLam a n t <%> takeIrrefutable x1

  XLet a n p q
    -> XLet a n p <%> takeIrrefutable q

  XValue a (SumT t _ ) (VLeft x)  -> Just (Left (XValue a t x))
  XValue a (SumT _ t ) (VRight x) -> Just (Right (XValue a t x))
  XValue{} -> Nothing
  XVar{}   -> Nothing
  XPrim{}  -> Nothing
 where
  x <%> a = fmap (either (Left . x) (Right . x)) a

unshuffleLets :: (Monad m, Hashable n, Eq n)
              => a -> C.Exp a n -> FixT m (C.Exp a n)
unshuffleLets _ = go
  where
    go xx = case xx of
      XApp a p q
        -> do px <- go p
              qx <- go q
              return (XApp a px qx)

      XLet a n b q
        -> do bx <- go b
              qx <- go q
              case bx of
                XLet a1 n1 b1 x1
                  -> progress
                     $ XLet a1 n1 b1
                     $ XLet a n x1 qx
                _ -> return
                     $ XLet a n bx qx

      XLam a n t x
        -> do x1 <- go x
              return $ XLam a n t x1

      XVar{}   -> return xx
      XPrim{}  -> return xx
      XValue{} -> return xx
