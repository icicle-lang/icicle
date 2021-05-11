-- | Generating Fresh names
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE ExistentialQuantification #-}
module Icicle.Common.Fresh (
      FreshT   (..)
    , Fresh
    , runFresh
    , runFreshIdentity
    , NameState
    , mkNameState
    , counterNameState
    , counterPrefixNameState
    , fresh
    , freshBase
    , freshPrefix
    , freshPrefixBase
    ) where

import              Icicle.Common.Base

import              P

import              Control.Monad.Morph
import              Data.Functor.Identity
import              Data.Hashable

newtype FreshT n m a
 = FreshT
 { runFreshT :: NameState n -> m (NameState n, a) }

type Fresh n = FreshT n Identity

runFresh :: Fresh n a -> NameState n -> (NameState n, a)
runFresh f ns
 = runIdentity
 $ runFreshT f ns


runFreshIdentity :: Monad m => Fresh n x -> FreshT n m x
runFreshIdentity
 = hoist generalize

--------------------------------------------------------------------------------

data NameState n
 = forall s. NameState (s -> (NameBase n, s)) !s

-- we don't care about sequencing the name state, just need this for
-- functions of type FreshT n m a. The `a` is the only thing we care
-- about.
instance NFData (NameState n) where rnf _ = ()

mkNameState :: (s -> (NameBase n, s)) -> s -> NameState n
mkNameState = NameState

counterNameState :: (Int -> NameBase n) -> Int -> NameState n
counterNameState f
 = mkNameState (\i' -> (f i', i' + 1))

counterPrefixNameState :: (Int -> n) -> n -> NameState n
counterPrefixNameState print prefix
 = counterNameState (NameMod prefix . NameBase . print) 0

fresh :: (Hashable n, Monad m) => FreshT n m (Name n)
fresh = nameOf <$> freshBase

freshBase :: Monad m => FreshT n m (NameBase n)
freshBase
 = FreshT
 $ \(NameState step state)
 -> let (n, s) = step state
    in  return (NameState step s, n)

freshPrefix :: (Hashable n, Monad m) => n -> FreshT n m (Name n)
freshPrefix pre
 = freshPrefixBase (NameBase pre)

freshPrefixBase :: (Hashable n, Monad m) => NameBase n -> FreshT n m (Name n)
freshPrefixBase pre
 = nameOf . prefix pre <$> freshBase
 where
  prefix (NameBase a)   b = NameMod a b
  prefix (NameMod  a b) c = NameMod a (prefix b c)

--------------------------------------------------------------------------------

instance Monad m => Monad (FreshT n m) where
 (>>=) p q
  = FreshT
  $ \ns
  -> do (ns',a) <- runFreshT p ns
        runFreshT (q a) ns'

 return a = FreshT $ \ns -> return (ns, a)

instance Monad m => Functor (FreshT n m) where
 fmap = liftM

instance Monad m => Applicative (FreshT n m) where
 pure = return
 (<*>) = ap

instance MonadTrans (FreshT n) where
 lift m
  = FreshT
  $ \ns
  -> do v <- m
        return (ns, v)

instance MFunctor (FreshT n) where
  hoist f (FreshT p) =
    FreshT $
      f . p
