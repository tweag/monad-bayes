{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Dynamic
-- Description : Distributions on execution traces that can be dynamically frozen
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Dynamic
  ( TracedT,
    hoist,
    marginal,
    freeze,
    mhStep,
    mh,
  )
where

import Control.Monad (join)
import Control.Monad.Bayes.Class
  ( MonadDistribution (random),
    MonadFactor (..),
    MonadMeasure,
  )
import Control.Monad.Bayes.Density.Free (DensityT)
import Control.Monad.Bayes.Traced.Common
  ( Trace (..),
    bind,
    mhTransFree,
    scored,
    singleton,
  )
import Control.Monad.Bayes.Weighted (WeightedT)
import Control.Monad.Trans (MonadTrans (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)

-- | A tracing monad where only a subset of random choices are traced and this
-- subset can be adjusted dynamically.
newtype TracedT m a = TracedT {runTraced :: m (WeightedT (DensityT m) a, Trace a)}

pushM :: (Monad m) => m (WeightedT (DensityT m) a) -> WeightedT (DensityT m) a
pushM = join . lift . lift

instance (Monad m) => Functor (TracedT m) where
  fmap f (TracedT c) = TracedT $ do
    (m, t) <- c
    let m' = fmap f m
    let t' = fmap f t
    return (m', t')

instance (Monad m) => Applicative (TracedT m) where
  pure x = TracedT $ pure (pure x, pure x)
  (TracedT cf) <*> (TracedT cx) = TracedT $ do
    (mf, tf) <- cf
    (mx, tx) <- cx
    return (mf <*> mx, tf <*> tx)

instance (Monad m) => Monad (TracedT m) where
  (TracedT cx) >>= f = TracedT $ do
    (mx, tx) <- cx
    let m = mx >>= pushM . fmap fst . runTraced . f
    t <- return tx `bind` (fmap snd . runTraced . f)
    return (m, t)

instance MonadTrans TracedT where
  lift m = TracedT $ fmap ((,) (lift $ lift m) . pure) m

instance (MonadDistribution m) => MonadDistribution (TracedT m) where
  random = TracedT $ fmap ((,) random . singleton) random

instance (MonadFactor m) => MonadFactor (TracedT m) where
  score w = TracedT $ fmap (score w,) (score w >> pure (scored w))

instance (MonadMeasure m) => MonadMeasure (TracedT m)

hoist :: (forall x. m x -> m x) -> TracedT m a -> TracedT m a
hoist f (TracedT c) = TracedT (f c)

-- | Discard the trace and supporting infrastructure.
marginal :: (Monad m) => TracedT m a -> m a
marginal (TracedT c) = fmap (output . snd) c

-- | Freeze all traced random choices to their current values and stop tracing
-- them.
freeze :: (Monad m) => TracedT m a -> TracedT m a
freeze (TracedT c) = TracedT $ do
  (_, t) <- c
  let x = output t
  return (return x, pure x)

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: (MonadDistribution m) => TracedT m a -> TracedT m a
mhStep (TracedT c) = TracedT $ do
  (m, t) <- c
  t' <- mhTransFree m t
  return (m, t')

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps.
mh :: (MonadDistribution m) => Int -> TracedT m a -> m [a]
mh n (TracedT c) = do
  (m, t) <- c
  let f k
        | k <= 0 = return (t :| [])
        | otherwise = do
            (x :| xs) <- f (k - 1)
            y <- mhTransFree m x
            return (y :| x : xs)
  fmap (map output . NE.toList) (f n)
