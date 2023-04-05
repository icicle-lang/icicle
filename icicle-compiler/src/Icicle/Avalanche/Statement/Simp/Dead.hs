{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE BangPatterns      #-}
-- Remove accumulators that don't contribute to an Output
module Icicle.Avalanche.Statement.Simp.Dead (
    dead
  , killAccumulators
  ) where

import              Icicle.Avalanche.Statement.Statement

import              Icicle.Common.Base
import              Icicle.Common.Exp

import              P

import              Data.Functor.Identity
import              Data.Set (Set)
import qualified    Data.Set as Set
import              Data.Map.Strict (Map)
import qualified    Data.Map.Strict as Map
import              Data.Hashable (Hashable)


-- | Remove accumulators that don't contribute to output
--
dead :: (Hashable n, Eq n) => Statement a n p -> Statement a n p
dead ss
 = let !(_,kills,ss') = deadS mempty ss
   in  killAccumulators kills ss'

data Usage n
 = Usage
 { usageAcc :: Set (Name n) -- accumulator names
 , usageExp :: Set (Name n) -- local names
 }

instance Eq n => Semigroup (Usage n) where
 a <> b
  = Usage
  { usageAcc = Set.union (usageAcc a) (usageAcc b)
  , usageExp = Set.union (usageExp a) (usageExp b)
  }

instance Eq n => Monoid (Usage n) where
 mempty = Usage Set.empty Set.empty
 mappend = (<>)

type Kill a n p = Map (Name n) (Exp a n p)

deadS :: (Hashable n, Eq n) => Usage n -> Statement a n p -> (Usage n, Kill a n p, Statement a n p)
deadS us statements
 = case statements of
    If x ts fs
     -- We run usage analysis sequentially instead of applying the
     -- accumulator each branch and then merging.
     --
     -- We do this because the set of used variables can be very large,
     -- and performing a join for every small if branch means we spend
     -- a huge amount of time doing set unions (60% of compilation).
     --
     -- It turns out this is fine (although less obvious), as locally
     -- bound variables are scoped, so liveness analysis doesn't float
     -- outside the branch, and usage of an accumulator is applicable
     -- if it's used in both or either branch.
     -> let (tU, tK, tS) = deadS us ts
            (fU, fK, fS) = deadS tU fs
            xU           = usageX x
        in (mconcat [fU, xU], mconcat [tK, fK], If x tS fS)
    Let n x ss
     -> let (sU, sK, sS) = deadS us ss
            xU           = usageX x
        in  if   usedX n sU
            then (deleteX n (mconcat [xU, sU]), sK, Let n x sS)
            else (sU, sK, sS)
    While t n nt end ss
     -- While is a bit special. We must ensure writes to the while condition are not removed,
     -- even if there appears to be no use in the body.
     -> let useN         = usageA n
            (sU, sK, sS) = deadLoop (us <> useN) ss
            eU           = usageX end
        in (mconcat [sU, eU, usageA n], Map.delete n sK, While t n nt end sS)
    ForeachInts t n from to ss
     -> let (sU, sK, sS) = deadLoop us ss
            fU           = usageX from
            tU           = usageX to
        in (mconcat [sU, fU, tU], sK, ForeachInts t n from to sS)
    ForeachFacts binds vt ss
     -> let (sU, sK, sS) = deadLoop us ss
        in  (sU, sK, ForeachFacts binds vt sS)

    Block []
     -> (us, mempty, statements)
    Block (t:rs)
     -> let (rU, rK, rS) = deadS us (Block rs)
            (tU, tK, tS) = deadS rU t
        in  case rS of
             Block rS' -> (tU, mconcat [rK, tK], Block (tS : rS'))
             _         -> (tU, mconcat [rK, tK], Block [tS, rS])

    InitAccumulator acc@(Accumulator n _ x) ss
     -> let (sU, sK, sS) = deadS us ss
            xU           = usageX x
        in  if   usedA n sU
            then (mconcat [xU, sU],  sK, InitAccumulator acc sS)
            else (sU, Map.insert n x sK, sS)

    Read nx na t ss
     -> let (sU, sK, sS) = deadS us ss
        in  if   usedX nx sU
            then (deleteX nx (mconcat [sU, usageA na]), sK, Read nx na t sS)
            else (sU, sK, sS)

    Write na x
     ->     if   usedA na us
            then (mconcat [us, usageX x], mempty, Write na x)
            else (us, mempty, mempty)

    Output n t xts
     -> let xsU = fmap (usageX.fst) xts
        in  (mconcat (us : xsU), mempty, Output n t xts)


-- | Find fixpoint of loop usage
-- Will this terminate?
-- Yes!
-- Given that we only ever insert into a usage map, we can say that
--
-- > forall ss usage.
-- > usage `subseteq` fst (deadS usage ss)
--
-- And assuming there are a finite number of variables in ss,
-- the worst case is that the usage is all variables in ss.
deadLoop :: (Hashable n, Eq n) => Usage n -> Statement a n p -> (Usage n, Kill a n p, Statement a n p)
deadLoop us ss
 = let (sU, sK, sS) = deadS us ss
   in  if   sU `eqUsage` us
       then (sU, sK, sS)
       -- Make sure to use the original statements
       else deadLoop sU ss
 where
  -- We can cheat when checking 'equality' of the usage sets:
  -- if they are the same size, they must contain the same elements.
  -- (Because one is obtained by only inserting elements to the other)
  eqUsage a b
   =  Set.size (usageAcc a) == Set.size (usageAcc b)
   && Set.size (usageExp a) == Set.size (usageExp b)


usageX :: (Hashable n, Eq n) => Exp a n p -> Usage n
usageX x = Usage mempty (freevars x)

usedX :: Eq n => Name n -> Usage n -> Bool
usedX n us = Set.member n (usageExp us)

deleteX :: Eq n => Name n -> Usage n -> Usage n
deleteX x us = us { usageExp = Set.delete x (usageExp us) }

usageA :: Eq n => Name n -> Usage n
usageA n = Usage (Set.singleton n) mempty

usedA :: Eq n => Name n -> Usage n -> Bool
usedA n us = Set.member n (usageAcc us)

killAccumulators :: Eq n => Kill a n p -> Statement a n p -> Statement a n p
killAccumulators accs statements
 = runIdentity
 $ transformUDStmt trans () statements
 where
  trans _ s
   | Read n acc _ ss <- s
   , Just xx         <- Map.lookup acc accs
   = return ((), Let n xx ss)
   | Write acc _ <- s
   , Map.member acc accs
   = return ((), mempty)

   | otherwise
   = return ((), s)
{-# INLINE killAccumulators #-}
