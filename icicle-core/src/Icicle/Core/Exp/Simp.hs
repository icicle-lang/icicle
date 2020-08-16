{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE PatternGuards     #-}
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

import           Control.Lens.Plated            (transformM)
import           Control.Monad.Trans.Class      (lift)

import           Data.Monoid                    (Sum (..))
import qualified Data.Set                       as Set
import           Data.Hashable                  (Hashable)

-- | Core Simplifier:
--   * a normal
--   * beta reduction
--   * constant folding fully applied primitives
--   * case of irrefutable case
--   * let unwinding
--   * irrefutable case reduction
--   * irrefutable case branches abstraction
simp :: (Hashable n, Eq n) => a -> C.Exp a n -> Fresh n (C.Exp a n)
simp a_fresh
  =   anormal a_fresh
  <=< fmap deadX
  .   fixpoint (allSimp a_fresh)

-- | Core Simplifier FixT loop.
allSimp :: (Hashable n, Eq n)
        => a -> C.Exp a n -> FixT (Fresh n) (C.Exp a n)
allSimp a_fresh
  =   caseOfScrutinisedCase a_fresh
  <=< transformM transformations
  .   B.beta

  where
    transformations
      =   simpX a_fresh
      >=> caseOfCase a_fresh
      >=> caseOfIrrefutable a_fresh
      >=> caseConstants a_fresh
      >=> unshuffleLets a_fresh
      >=> inlineLets a_fresh
      >=> ifShuffle a_fresh

-- | Constant folding for some primitives
simpX :: (Monad m, Hashable n, Eq n)
             => a -> C.Exp a n -> FixT m (C.Exp a n)
simpX a_fresh xx
  | Just (prim, as) <- takePrimApps xx
  , Just args       <- mapM takeValue as
  , Just simplified <- simpP a_fresh prim args
  = progress simplified
  | otherwise
  = return xx

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
      b@XPrim {}   -> (b, Set.empty)
      b@XValue {}  -> (b, Set.empty)

-- | Case of Irrefutable Case.
caseOfCase :: (Hashable n, Eq n)
           => a -> C.Exp a n -> FixT (Fresh n) (C.Exp a n)
caseOfCase a_fresh xx
  | Just (primitive, arguments) <- takePrimApps xx
  , PrimFold _ ret'typ <- primitive
  , [XLam _ leftName _ leftExpression, XLam _ rightName _ rightExpression, scrutinee] <- arguments
  , (scrutinee_lets, scrutinee_no_lets) <- takeLets scrutinee
  , Just (primitive', innerArguments) <- takePrimApps scrutinee_no_lets
  , PrimFold fld _ <- primitive'
  , [XLam _ innerLeftName innerLeftType innerLeftExpression, XLam _ innerRightName innerRightType innerRightExpression, innerScrutinee] <- innerArguments
  , Just (l'case, r'case) <- (,) <$> takeIrrefutable innerLeftExpression <*> takeIrrefutable innerRightExpression
  = do
      newLeftName <- lift fresh
      newRightName <- lift fresh

      let
        renameInLeft
          = join either $ subsNameInExp innerLeftName newLeftName

        renameInRight
          = join either $ subsNameInExp innerRightName newRightName

        replaceIn
          = either (const leftExpression) (const rightExpression)

        nameIn
          = either (const leftName) (const rightName)

        newLeftLam
          = xlam newLeftName innerLeftType
          $ xlet <$> nameIn <*> renameInLeft <*> replaceIn
          $ l'case

        newRightLam
          = xlam newRightName innerRightType
          $ xlet <$> nameIn <*> renameInRight <*> replaceIn
          $ r'case

      progress
        $ makeLets a_fresh scrutinee_lets
        $ xprim (PrimFold fld ret'typ)
          `xapp` newLeftLam
          `xapp` newRightLam
          `xapp` innerScrutinee

  | otherwise
  = return xx
    where
  xapp = XApp a_fresh
  xprim = XPrim a_fresh
  xlet = XLet a_fresh
  xlam = XLam a_fresh

-- | If the case scrutinee is irrefutable,
--   replace the case expression with a let
--   binding.
caseOfIrrefutable :: (Monad m, Hashable n, Eq n)
                  => a -> C.Exp a n -> FixT m (C.Exp a n)
caseOfIrrefutable a_fresh xx
  | Just (PrimFold _ _, [XLam _ l'n _ l'exp, XLam _ r'n _ r'exp, scrut]) <- takePrimApps xx
  , Just scrut' <- takeIrrefutable scrut
  = case scrut' of
      Left x ->
        progress $
          xlet l'n x l'exp
      Right x ->
        progress $
          xlet r'n x r'exp

  | otherwise
  = return xx
    where
  xlet = XLet a_fresh

-- | Simplification when both sides of a fold
--   are wrapped with the same constructor.
--
--   By itself this doesn't do much, but it does
--   permit the case of case optimisation and has
--   no real cost itself.
caseConstants :: (Monad m, Hashable n, Eq n)
              => a -> C.Exp a n -> FixT m (C.Exp a n)
caseConstants a_fresh xx
  | Just (PrimFold a (SumT ret'l ret'r), [XLam _ l'n l'typ l'exp, XLam _ r'n r'typ r'exp, scrut]) <- takePrimApps xx
  , Just (l'side, r'side) <- (,) <$> takeIrrefutable l'exp <*> takeIrrefutable r'exp
  = case (l'side, r'side) of
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
      _ -> return xx
  | otherwise
  = return xx
    where
  xapp = XApp a_fresh
  xprim = XPrim a_fresh
  xlam = XLam a_fresh
  xleft l r = xprim $ PrimMinimal (Min.PrimConst (Min.PrimConstLeft l r))
  xright l r = xprim $ PrimMinimal (Min.PrimConst (Min.PrimConstRight l r))

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
          | PrimMinimal (Min.PrimConst Min.PrimConstSome {}) <- prim
          , [real] <- as
          -> Just (Left real)
        _ -> Nothing

  XLam a n t x1
    -> XLam a n t <%> takeIrrefutable x1

  XLet a n p q
    -> XLet a n p <%> takeIrrefutable q

  XValue a (SumT t _ ) (VLeft x)  -> Just (Left  (XValue a t x))
  XValue a (SumT _ t ) (VRight x) -> Just (Right (XValue a t x))
  XValue a (OptionT t ) (VSome x) -> Just (Left  (XValue a t x))
  XValue a BoolT (VBool True)     -> Just (Left  (XValue a UnitT VUnit))
  XValue a BoolT (VBool False)    -> Just (Right (XValue a UnitT VUnit))
  XValue{} -> Nothing
  XVar{}   -> Nothing
  XPrim{}  -> Nothing
 where
  x <%> a = fmap (either (Left . x) (Right . x)) a

unshuffleLets :: (Hashable n, Eq n)
              => a -> C.Exp a n -> FixT (Fresh n) (C.Exp a n)
unshuffleLets _ xx
  | XLet a n b q <- xx
  , XLet a1 n1 b1 x1 <- b
  = do
    n1'new <- lift fresh
    progress
      $ XLet a1 n1'new b1
      $ XLet a n (subsNameInExp n1 n1'new x1) q

  | otherwise
  = return xx

-- | If we've already scrutinised an expression, don't check it
--   again in either of its branches.
caseOfScrutinisedCase :: (Monad m, Hashable n, Eq n) => a -> C.Exp a n -> FixT m (C.Exp a n)
caseOfScrutinisedCase a_fresh = go []
  where
    go seen x = case x of
      XApp a p q
        | Just (fld@(PrimFold _ _), [XLam la lname ltyp lexp, XLam lb rname rtyp rexp, scrut]) <- takePrimApps x
        -> do lexp'  <- go ((scrut, Left lname) : seen) lexp
              rexp'  <- go ((scrut, Right rname) : seen) rexp
              case find (\(e,_) -> e `simpleEquality` scrut) seen of
                Just (_,replacement) ->
                  progress
                  $ either
                    (\n -> subsNameInExp lname n lexp')
                    (\n -> subsNameInExp rname n rexp')
                    replacement

                Nothing -> do
                  scrut' <- go seen scrut
                  return
                    $ xprim fld
                     `xapp` XLam la lname ltyp lexp'
                     `xapp` XLam lb rname rtyp rexp'
                     `xapp` scrut'

        | otherwise
        -> XApp a <$> go seen p <*> go seen q

      XLam a n t p
        -> XLam a n t <$> go seen p

      XLet a n p q ->
        XLet a n <$> go seen p <*> go seen q

      XVar {}   -> pure x
      XPrim {}  -> pure x
      XValue {} -> pure x

    xapp = XApp a_fresh
    xprim = XPrim a_fresh

-- | Inline let bindings which only have
--   a single use, and are likely to be
--   able to be optimised away.
--
--   If the binding is irrefutable, or could
--   be part of the case in case optimisation,
--   inline it instead.
inlineLets :: (Hashable n, Eq n)
                  => a -> C.Exp a n -> FixT (Fresh n) (C.Exp a n)
inlineLets _ xx
  | XLet a n p q <- xx
  , casesIrrefutable p
  , varCount n q == 1
  = do new <- lift $ subst1 a n p q
       progress new
  | otherwise
  = return xx
 where
  casesIrrefutable scrut
    | Just _ <- takeIrrefutable scrut
    = True
    | Just (primitive, ass) <- takePrimApps scrut
    , PrimFold _ _ <- primitive
    , [XLam _ _ _ l, XLam _ _ _ r, _] <- ass
    , Just _ <- (,) <$> takeIrrefutable l <*> takeIrrefutable r
    = True
    | otherwise
    = False

-- | Merge pathological if blocks
--
-- ```
-- if#
--   (\_ ->
--     if#
--       (\_ -> Right Result))
--       (\_ -> Left ExceptCannotCompute)
--       (InnerScrut))
--   (\_ -> Left ExceptCannotCompute)
--   OuterScut
-- ```
--
-- to
--
-- ```
-- let
--   Scrut =
--     and# OuterScut InnerScrut
-- in
--   if#
--     (\_ -> Right Result))
--     (\_ -> Left ExceptCannotCompute)
--     Scrut
-- ```
--
-- With the other optimisations, this essentially means that
-- big maps can be created in the precomputation, and all of the
-- map insert binary search logic will be elided and replaced
-- with a constant.
--
-- This works very well, but is brittle against source level
-- let bindings, which prevent inlining from occurring.
ifShuffle :: (Hashable n, Eq n)
          => a -> C.Exp a n -> FixT (Fresh n) (C.Exp a n)
ifShuffle a_fresh xx
  | Just (PrimFold PrimFoldBool ret'typ, [XLam _ tN _ toExp, fExp@(XLam _ _ _ foExp), oScrutinee]) <- takePrimApps xx
  , Just (PrimFold PrimFoldBool _,       [tExp,                    XLam _ _ _ fiExp , iScrutinee]) <- takePrimApps toExp
  , foExp `alphaEquality` fiExp
  = do let
         n'Scrutinee =
           xand `xapp` oScrutinee `xapp` iScrutinee

       progress $
         xlet tN (xvalue UnitT VUnit) $
         xprim (PrimFold PrimFoldBool ret'typ) `xapp` tExp `xapp` fExp `xapp` n'Scrutinee

  | otherwise
  = return xx

 where
    xvalue = XValue a_fresh
    xlet   = XLet   a_fresh
    xapp   = XApp   a_fresh
    xprim  = XPrim  a_fresh
    xand   = xprim $ PrimMinimal (Min.PrimLogical Min.PrimLogicalAnd)

subsNameInExp :: Eq n => Name n -> Name n -> Exp a n p -> Exp a n p
subsNameInExp old new =
  let worker m | m == old  = new
               | otherwise = m
  in renameExp worker

varCount :: Eq n => Name n -> Exp a n p -> Sum Int
varCount i (XVar _ j)
  | i == j   = Sum 1
varCount i x = foldExp (varCount i) x
