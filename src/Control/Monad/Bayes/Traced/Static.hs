{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Static
-- Description : Distributions on execution traces of full programs
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Static
  ( TracedT (..),
    hoist,
    marginal,
    mhStep,
    mh,
  )
where

import Control.Applicative (liftA2)
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

-- | A tracing monad where only a subset of random choices are traced.
--
-- The random choices that are not to be traced should be lifted from the
-- transformed monad.
data TracedT m a = TracedT
  { model :: WeightedT (DensityT m) a,
    traceDist :: m (Trace a)
  }

instance (Monad m) => Functor (TracedT m) where
  fmap f (TracedT m d) = TracedT (fmap f m) (fmap (fmap f) d)

instance (Monad m) => Applicative (TracedT m) where
  pure x = TracedT (pure x) (pure (pure x))
  (TracedT mf df) <*> (TracedT mx dx) = TracedT (mf <*> mx) (liftA2 (<*>) df dx)

instance (Monad m) => Monad (TracedT m) where
  (TracedT mx dx) >>= f = TracedT my dy
    where
      my = mx >>= model . f
      dy = dx `bind` (traceDist . f)

instance MonadTrans TracedT where
  lift m = TracedT (lift $ lift m) (fmap pure m)

instance (MonadDistribution m) => MonadDistribution (TracedT m) where
  random = TracedT random (fmap singleton random)

instance (MonadFactor m) => MonadFactor (TracedT m) where
  score w = TracedT (score w) (score w >> pure (scored w))

instance (MonadMeasure m) => MonadMeasure (TracedT m)

hoist :: (forall x. m x -> m x) -> TracedT m a -> TracedT m a
hoist f (TracedT m d) = TracedT m (f d)

-- | Discard the trace and supporting infrastructure.
marginal :: (Monad m) => TracedT m a -> m a
marginal (TracedT _ d) = fmap output d

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: (MonadDistribution m) => TracedT m a -> TracedT m a
mhStep (TracedT m d) = TracedT m d'
  where
    d' = d >>= mhTransFree m

-- $setup
-- >>> import Control.Monad.Bayes.Class
-- >>> import Control.Monad.Bayes.Sampler.Strict
-- >>> import Control.Monad.Bayes.Weighted

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps. Newest samples are at the head of the list.
--
-- For example:
--
-- * I have forgotten what day it is.
-- * There are ten buses per hour in the week and three buses per hour at the weekend.
-- * I observe four buses in a given hour.
-- * What is the probability that it is the weekend?
--
-- >>> :{
--  let
--    bus = do x <- bernoulli (2/7)
--             let rate = if x then 3 else 10
--             factor $ poissonPdf rate 4
--             return x
--    mhRunBusSingleObs = do
--      let nSamples = 2
--      sampleIOfixed $ unweighted $ mh nSamples bus
--  in mhRunBusSingleObs
-- :}
-- [True,True,True]
--
-- Of course, it will need to be run more than twice to get a reasonable estimate.
mh :: (MonadDistribution m) => Int -> TracedT m a -> m [a]
mh n (TracedT m d) = fmap (map output . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) d
      | otherwise = do
          (x :| xs) <- f (k - 1)
          y <- mhTransFree m x
          return (y :| x : xs)
