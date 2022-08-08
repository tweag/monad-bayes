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

import Control.Monad.Bayes.Class (MonadInfer, MonadSample)
import Control.Monad.Bayes.Population
  ( Population,
    pushEvidence,
    withParticles,
  )
import Control.Monad.Bayes.Sequential as Seq
  ( Sequential,
    hoistFirst,
    sequentially,
  )

data SMCConfig m = SMCConfig
  { resampler :: forall x. Population m x -> Population m x,
    numSteps :: Int,
    numParticles :: Int
  }

-- | Sequential importance resampling.
-- Basically an SMC template that takes a custom resampler.
<<<<<<< HEAD
sir ::
  (Monad (m n), RealFloat n) =>
  -- | resampler
  (forall x. Population m n x -> Population m n x) ->
  -- | number of timesteps
  Int ->
  -- | population size
  Int ->
  -- | model
  Sequential (Population m) n a ->
  Population m n a
sir resampler k n = sis resampler k . Seq.hoistFirst (spawn n >>)

-- | Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are not normalized.
smcMultinomial ::
  (MonadSample n m, RealFloat n) =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | model
  Sequential (Population m) n a ->
  Population m n a
smcMultinomial = sir resampleMultinomial

-- | Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are not normalized.
smcSystematic ::
  (MonadSample n m, RealFloat n) =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | model
  Sequential (Population m) n a ->
  Population m n a
smcSystematic = sir resampleSystematic

-- | Sequential Monte Carlo with stratified resampling at each timestep.
-- Weights are not normalized.
smcStratified ::
  (MonadSample n m, RealFloat n) =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  Sequential (Population m) n a ->
  -- | model
  Population m n a
smcStratified = sir resampleStratified
=======
smc ::
  MonadSample m =>
  SMCConfig m ->
  Sequential (Population m) a ->
  Population m a
smc SMCConfig {..} = sequentially resampler numSteps . Seq.hoistFirst (withParticles numParticles)
>>>>>>> api

-- | Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
<<<<<<< HEAD
smcMultinomialPush ::
  (MonadInfer n m, RealFloat n) =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | model
  Sequential (Population m) n a ->
  Population m n a
smcMultinomialPush = sir (pushEvidence . resampleMultinomial)

-- | Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
smcSystematicPush ::
  (MonadInfer n m, RealFloat n) =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | model
  Sequential (Population m) n a ->
  Population m n a
smcSystematicPush = sir (pushEvidence . resampleSystematic)
=======
smcPush ::
  MonadInfer m => SMCConfig m -> Sequential (Population m) a -> Population m a
smcPush config = smc config {resampler = (pushEvidence . resampler config)}
>>>>>>> api
