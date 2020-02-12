{-|
Module      : Control.Monad.Bayes.Inference.SMC
Description : Sequential Monte Carlo
Copyright   : (c) Adam Scibior, 2015-2020
License     : MIT
Maintainer  : leonhard.markert@tweag.io
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Inference.SMC (
  sir,
  smcMultinomial,
  smcSystematic,
  smcMultinomialPush,
  smcSystematicPush
) where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sequential as Seq

-- | Sequential importance resampling.
-- Basically an SMC template that takes a custom resampler.
sir :: Monad m
    => (forall x. Population m x -> Population m x) -- ^ resampler
    -> Int -- ^ number of timesteps
    -> Int -- ^ population size
    -> Sequential (Population m) a -- ^ model
    -> Population m a
sir resampler k n = sis resampler k . Seq.hoistFirst (spawn n >>)

-- | Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are not normalized.
smcMultinomial :: MonadSample m
               => Int -- ^ number of timesteps
               -> Int -- ^ number of particles
               -> Sequential (Population m) a -- ^ model
               -> Population m a
smcMultinomial = sir resampleMultinomial

-- | Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are not normalized.
smcSystematic  :: MonadSample m
               => Int -- ^ number of timesteps
               -> Int -- ^ number of particles
               -> Sequential (Population m) a -- ^ model
               -> Population m a
smcSystematic = sir resampleSystematic

-- | Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
smcMultinomialPush :: MonadInfer m
                   => Int -- ^ number of timesteps
                   -> Int -- ^ number of particles
                   -> Sequential (Population m) a -- ^ model
                   -> Population m a
smcMultinomialPush = sir (pushEvidence . resampleMultinomial)

-- | Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
smcSystematicPush  :: MonadInfer m
                   => Int -- ^ number of timesteps
                   -> Int -- ^ number of particles
                   -> Sequential (Population m) a -- ^ model
                   -> Population m a
smcSystematicPush = sir (pushEvidence . resampleSystematic)
