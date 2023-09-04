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
  ( Population,
    pushEvidence,
    withParticles,
  )
import Control.Monad.Bayes.Sequential.Coroutine as Coroutine

data SMCConfig m = SMCConfig
  { resampler :: forall x. Population m x -> Population m x,
    numSteps :: Int,
    numParticles :: Int
  }

-- | Sequential importance resampling.
-- Basically an SMC template that takes a custom resampler.
smc ::
  (MonadDistribution m) =>
  SMCConfig m ->
  Coroutine.Sequential (Population m) a ->
  Population m a
smc SMCConfig {..} =
  Coroutine.sequentially resampler numSteps
    . Coroutine.hoistFirst (withParticles numParticles)

-- | Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
smcPush ::
  (MonadMeasure m) => SMCConfig m -> Coroutine.Sequential (Population m) a -> Population m a
smcPush config = smc config {resampler = (pushEvidence . resampler config)}
