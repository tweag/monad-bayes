{-|
Module      : Control.Monad.Bayes.Inference.SMC
Description : Sequential Monte Carlo
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
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
smcMultinomial k n = sir resampleMultinomial k n

-- | Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are not normalized.
smcSystematic  :: MonadSample m
               => Int -- ^ number of timesteps
               -> Int -- ^ number of particles
               -> Sequential (Population m) a -- ^ model
               -> Population m a
smcSystematic k n = sir resampleSystematic k n

-- | Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a factor into the transformed monad.
smcMultinomialPush :: MonadInfer m
                   => Int -- ^ number of timesteps
                   -> Int -- ^ number of particles
                   -> Sequential (Population m) a -- ^ model
                   -> Population m a
smcMultinomialPush k n = sir (pushEvidence . resampleMultinomial) k n

-- | Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a factor into the transformed monad.
smcSystematicPush  :: MonadInfer m
                   => Int -- ^ number of timesteps
                   -> Int -- ^ number of particles
                   -> Sequential (Population m) a -- ^ model
                   -> Population m a
smcSystematicPush k n = sir (pushEvidence . resampleSystematic) k n
