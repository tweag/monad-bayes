{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Control.Monad.Bayes.Free
-- Description : Free monad transformer over random sampling
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- 'FreeSampler' is a free monad transformer over random sampling.
module Control.Monad.Bayes.Free
  ( FreeSampler,
    hoist,
    interpret,
    withRandomness,
    withPartialRandomness,
    traced,
  )
where

import Control.Monad.Bayes.Class (MonadSample (..))
import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (FT, MonadFree (..), hoistFT, iterT, iterTM, liftF)
import Control.Monad.Writer (WriterT (..), tell)
import Data.Functor.Identity (Identity, runIdentity)
import Prelude hiding (Real)

-- | Random sampling functor.
newtype SamF n a = Random (n -> a)

instance Functor (SamF n) where
  fmap f (Random k) = Random (f . k)

-- | Free monad transformer over random sampling.
--
-- Uses the Church-encoded version of the free monad for efficiency.
newtype FreeSampler m a = FreeSampler {runFreeSampler :: FT (SamF (Real m)) m a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans (FreeSampler)

instance (Real m ~ n, RealFloat (n)) => MonadFree (SamF n) (FreeSampler m) where
  wrap = FreeSampler . wrap . fmap runFreeSampler

instance (Monad m, RealFloat (Real m)) => MonadSample (FreeSampler m) where
  type Real (FreeSampler m) = Real m
  random = FreeSampler $ liftF (Random id)

-- | Hoist 'FreeSampler' through a monad transform.
hoist :: (Real m ~ Real n, Monad m, Monad n) => (forall x. m x -> n x) -> FreeSampler m a -> FreeSampler n a
hoist f (FreeSampler m) = FreeSampler (hoistFT f m)

-- | Execute random sampling in the transformed monad.
interpret :: MonadSample m => FreeSampler m a -> m a
interpret (FreeSampler m) = iterT f m
  where
    f (Random k) = random >>= k

-- | Execute computation with supplied values for random choices.
withRandomness :: Monad m => [Real m] -> FreeSampler m a -> m a
withRandomness randomness (FreeSampler m) = evalStateT (iterTM f m) randomness
  where
    f (Random k) = do
      xs <- get
      case xs of
        [] -> error "FreeSampler: the list of randomness was too short"
        y : ys -> put ys >> k y

-- | Execute computation with supplied values for a subset of random choices.
-- Return the output value and a record of all random choices used, whether
-- taken as input or drawn using the transformed monad.
withPartialRandomness :: MonadSample m => [Real m] -> FreeSampler m a -> m (a, [Real m])
withPartialRandomness randomness (FreeSampler m) =
  runWriterT $ evalStateT (iterTM f $ hoistFT lift m) randomness
  where
    f (Random k) = do
      -- This block runs in StateT [Double] (WriterT [Double]) m.
      -- StateT propagates consumed randomness while WriterT records
      -- randomness used, whether old or new.
      xs <- get
      x <- case xs of
        [] -> random
        y : ys -> put ys >> return y
      tell [x]
      k x

-- | Like 'withPartialRandomness', but use an arbitrary sampling monad.
traced :: MonadSample m => [Real m] -> FreeSampler Identity a -> m (a, [Real m])
traced randomness m = undefined -- withPartialRandomness randomness $ hoist (return . runIdentity) m
