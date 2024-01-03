{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Control.Monad.Bayes.Inference.SMC
-- Description : Sequential Monte Carlo (SMC)
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- Sequential Monte Carlo (SMC) sampling.
--
-- Arnaud Doucet and Adam M. Johansen. 2011. A tutorial on particle filtering and smoothing: fifteen years later. In /The Oxford Handbook of Nonlinear Filtering/, Dan Crisan and Boris Rozovskii (Eds.). Oxford University Press, Chapter 8.
module Control.Monad.Bayes.Inference.SMC
  ( smc,
    smcPush,
    SMCConfig (..),
  )
where

import Control.Monad.Bayes.Class (MonadDistribution, MonadMeasure)
import Control.Monad.Bayes.Population
  ( PopulationT (..),
    flatten,
    pushEvidence,
    single,
    withParticles,
  )
import Control.Monad.Bayes.Population.Applicative qualified as Applicative
import Control.Monad.Bayes.Sequential.Coroutine as Coroutine
import Control.Monad.Bayes.Sequential.Coroutine qualified as SequentialT
import Control.Monad.Bayes.Weighted (WeightedT (..), weightedT)
import Control.Monad.Coroutine
import Control.Monad.Trans.Free (FreeF (..), FreeT (..))

data SMCConfig m = SMCConfig
  { resampler :: forall x. PopulationT m x -> PopulationT m x,
    numSteps :: Int,
    numParticles :: Int
  }

sequentialToPopulation :: (Monad m) => Coroutine.SequentialT (Applicative.PopulationT m) a -> PopulationT m a
sequentialToPopulation =
  PopulationT
    . weightedT
    . coroutineToFree
    . Coroutine.runSequentialT
  where
    coroutineToFree =
      FreeT
        . fmap (Free . fmap (\(cont, p) -> either (coroutineToFree . extract) (pure . (,p)) cont))
        . Applicative.runPopulationT
        . resume

-- | Sequential importance resampling.
-- Basically an SMC template that takes a custom resampler.
smc ::
  (MonadDistribution m) =>
  SMCConfig m ->
  Coroutine.SequentialT (PopulationT m) a ->
  PopulationT m a
smc SMCConfig {..} =
  (single . flatten)
    . Coroutine.sequentially resampler numSteps
    . SequentialT.hoist (single . flatten)
    . Coroutine.hoistFirst (withParticles numParticles)
    . SequentialT.hoist (single . flatten)

-- | Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
smcPush ::
  (MonadMeasure m) => SMCConfig m -> Coroutine.SequentialT (PopulationT m) a -> PopulationT m a
smcPush config = smc config {resampler = (single . flatten . pushEvidence . resampler config)}
