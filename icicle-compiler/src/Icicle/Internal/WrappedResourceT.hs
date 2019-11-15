-- | Extremely silly module to add back MonadBaseControl IO
--   to ResourceT.
--
--   We use lifted async for concurrency inside Icicle, and
--   need both exception handling and multi-threading.=
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Icicle.Internal.WrappedResourceT (
  WrappedResourceT (..)
) where

import           P

import           Control.Monad.Catch (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Resource.Internal (ResourceT (..), MonadResource)
import           Control.Monad.Trans.Control (MonadTransControl (..),MonadBaseControl (..), defaultRestoreM)
import           Control.Monad.Base (MonadBase, liftBase)

newtype WrappedResourceT m a =
  WrappedResourceT {
    usingResourceT :: ResourceT m a
  } deriving (Functor, Applicative, Monad, MonadResource, MonadIO, MonadTrans, MonadThrow, MonadCatch)

instance MonadBase b m => MonadBase b (WrappedResourceT m) where
    liftBase = lift . liftBase

instance MonadTransControl WrappedResourceT where
    type StT WrappedResourceT a = a
    liftWith f = WrappedResourceT $ ResourceT $ \r -> f $ \(WrappedResourceT (ResourceT t)) -> t r
    restoreT = WrappedResourceT . ResourceT . const

instance MonadBaseControl b m => MonadBaseControl b (WrappedResourceT m) where
    type StM (WrappedResourceT m) a = StM m a
    liftBaseWith f = WrappedResourceT $
      ResourceT $ \reader' ->
        liftBaseWith $ \runInBase ->
          f $ runInBase . (\(WrappedResourceT (ResourceT r)) -> r reader'  )
    restoreM = defaultRestoreM
