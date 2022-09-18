{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Control.Monad.Bayes.TracedT.Static
-- Description : Distributions on execution traces of full programs
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.TracedT.Static
  ( TracedT (..),
    hoist,
    marginal,
    mhStep,
    mh,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Bayes.Class
  ( MonadCond (..),
    MonadInfer,
    MonadSample (random),
  )
import Control.Monad.Bayes.Density.Free (Density)
import Control.Monad.Bayes.TracedT.Common
  ( Trace (..),
    bind,
    mhTransFree,
    scored,
    singleton,
  )
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Trans (MonadTrans (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)

-- | A tracing monad where only a subset of random choices are traced.
--
-- The random choices that are not to be traced should be lifted from the
-- transformed monad.
data TracedT m a = TracedT
  { model :: Weighted (Density m) a,
    traceDist :: m (Trace a)
  }

instance Monad m => Functor (TracedT m) where
  fmap f (TracedT m d) = TracedT (fmap f m) (fmap (fmap f) d)

instance Monad m => Applicative (TracedT m) where
  pure x = TracedT (pure x) (pure (pure x))
  (TracedT mf df) <*> (TracedT mx dx) = TracedT (mf <*> mx) (liftA2 (<*>) df dx)

instance Monad m => Monad (TracedT m) where
  (TracedT mx dx) >>= f = TracedT my dy
    where
      my = mx >>= model . f
      dy = dx `bind` (traceDist . f)

instance MonadTrans TracedT where
  lift m = TracedT (lift $ lift m) (fmap pure m)

instance MonadSample m => MonadSample (TracedT m) where
  random = TracedT random (fmap singleton random)

instance MonadCond m => MonadCond (TracedT m) where
  score w = TracedT (score w) (score w >> pure (scored w))

instance MonadInfer m => MonadInfer (TracedT m)

hoist :: (forall x. m x -> m x) -> TracedT m a -> TracedT m a
hoist f (TracedT m d) = TracedT m (f d)

-- | Discard the trace and supporting infrastructure.
marginal :: Monad m => TracedT m a -> m a
marginal (TracedT _ d) = fmap output d

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: MonadSample m => TracedT m a -> TracedT m a
mhStep (TracedT m d) = TracedT m d'
  where
    d' = d >>= mhTransFree m

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps. Newest samples are at the head of the list.
mh :: MonadSample m => Int -> TracedT m a -> m [a]
mh n (TracedT m d) = fmap (map output . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) d
      | otherwise = do
        (x :| xs) <- f (k - 1)
        y <- mhTransFree m x
        return (y :| x : xs)
