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
    smcMultinomial,
    smcSystematic,
    smcStratified,
    smcMultinomialPush,
    smcSystematicPush,
    smc',
    SMCConfig (..),
  )
where

import Control.Monad.Bayes.Class (MonadInfer, MonadSample)
import Control.Monad.Bayes.Population
  ( Population,
    pushEvidence,
    resampleMultinomial,
    resampleStratified,
    resampleSystematic,
    spawn,
  )
import Control.Monad.Bayes.Sequential as Seq
  ( Sequential,
    hoistFirst,
    sequentially,
  )

data SMCConfig = SMCConfig
  { resampler :: (forall x m. MonadSample m => Population m x -> Population m x),
    numSteps :: Int,
    numParticles :: Int
  }

-- | Sequential importance resampling.
-- Basically an SMC template that takes a custom resampler.
smc ::
  Monad m =>
  -- | resampler
  (forall x. Population m x -> Population m x) ->
  -- | number of timesteps
  Int ->
  -- | population size
  Int ->
  -- | model
  Sequential (Population m) a ->
  Population m a
smc resampler k n = sequentially resampler k . Seq.hoistFirst (spawn n >>)

smc' ::
  MonadSample m =>
  SMCConfig ->
  Sequential (Population m) a ->
  Population m a
smc' SMCConfig {..} = sequentially resampler numSteps . Seq.hoistFirst (spawn numParticles >>)

-- | Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are not normalized.
smcMultinomial,
  smcSystematic,
  smcStratified ::
    MonadSample m =>
    -- | number of timesteps
    Int ->
    -- | number of particles
    Int ->
    -- | model
    Sequential (Population m) a ->
    Population m a
smcMultinomial = smc resampleMultinomial

-- | Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are not normalized.
smcSystematic = smc resampleSystematic

-- | Sequential Monte Carlo with stratified resampling at each timestep.
-- Weights are not normalized.
smcStratified = smc resampleStratified

-- | Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
smcMultinomialPush,
  smcSystematicPush ::
    MonadInfer m =>
    -- | number of timesteps
    Int ->
    -- | number of particles
    Int ->
    -- | model
    Sequential (Population m) a ->
    Population m a
smcMultinomialPush = smc (pushEvidence . resampleMultinomial)

-- | Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
smcSystematicPush = smc (pushEvidence . resampleSystematic)
