{-# LANGUAGE RankNTypes #-}

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
  ( sir,
    smcMultinomial,
    smcSystematic,
    smcStratified,
    smcMultinomialPush,
    smcSystematicPush,
  )
where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sequential as Seq

-- | Sequential importance resampling.
-- Basically an SMC template that takes a custom resampler.
sir ::
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
sir resampler k n = sis resampler k . Seq.hoistFirst (spawn n >>)

-- | Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are not normalized.
smcMultinomial ::
  MonadSample m =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | model
  Sequential (Population m) a ->
  Population m a
smcMultinomial = sir resampleMultinomial

-- | Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are not normalized.
smcSystematic ::
  MonadSample m =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | model
  Sequential (Population m) a ->
  Population m a
smcSystematic = sir resampleSystematic

-- | Sequential Monte Carlo with stratified resampling at each timestep.
-- Weights are not normalized.
smcStratified ::
  MonadSample m =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  Sequential (Population m) a ->
  -- | model
  Population m a
smcStratified = sir resampleStratified

-- | Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
smcMultinomialPush ::
  MonadInfer m =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | model
  Sequential (Population m) a ->
  Population m a
smcMultinomialPush = sir (pushEvidence . resampleMultinomial)

-- | Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
smcSystematicPush ::
  MonadInfer m =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | model
  Sequential (Population m) a ->
  Population m a
smcSystematicPush = sir (pushEvidence . resampleSystematic)
