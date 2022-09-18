{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Control.Monad.Bayes.Inference.SMC2
-- Description : SequentialT Monte Carlo squared (SMC²)
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- SequentialT Monte Carlo squared (SMC²) sampling.
--
-- Nicolas Chopin, Pierre E. Jacob, and Omiros Papaspiliopoulos. 2013. SMC²: an efficient algorithm for sequential analysis of state space models. /Journal of the Royal Statistical Society Series B: Statistical Methodology/ 75 (2013), 397-426. Issue 3. <https://doi.org/10.1111/j.1467-9868.2012.01046.x>
module Control.Monad.Bayes.Inference.SMC2
  ( smc2,
    SMC2,
  )
where

import Control.Monad.Bayes.Class
  ( MonadCond (..),
    MonadInfer,
    MonadSample (random),
  )
import Control.Monad.Bayes.Inference.MCMC
import Control.Monad.Bayes.Inference.RMSMC (rmsmc)
import Control.Monad.Bayes.Inference.SMC (SMCConfig (SMCConfig, numParticles, numSteps, resampler), smcPush)
import Control.Monad.Bayes.PopulationT as Pop (PopulationT, population, resampleMultinomial)
import Control.Monad.Bayes.SequentialT (SequentialT)
import Control.Monad.Bayes.TracedT
import Control.Monad.Trans (MonadTrans (..))
import Numeric.Log (Log)

-- | Helper monad transformer for preprocessing the model for 'smc2'.
newtype SMC2 m a = SMC2 (SequentialT (TracedT (PopulationT m)) a)
  deriving newtype (Functor, Applicative, Monad)

setup :: SMC2 m a -> SequentialT (TracedT (PopulationT m)) a
setup (SMC2 m) = m

instance MonadTrans SMC2 where
  lift = SMC2 . lift . lift . lift

instance MonadSample m => MonadSample (SMC2 m) where
  random = lift random

instance Monad m => MonadCond (SMC2 m) where
  score = SMC2 . score

instance MonadSample m => MonadInfer (SMC2 m)

-- | SequentialT Monte Carlo squared.
smc2 ::
  MonadSample m =>
  -- | number of time steps
  Int ->
  -- | number of inner particles
  Int ->
  -- | number of outer particles
  Int ->
  -- | number of MH transitions
  Int ->
  -- | model parameters
  SequentialT (TracedT (PopulationT m)) b ->
  -- | model
  (b -> SequentialT (PopulationT (SMC2 m)) a) ->
  PopulationT m [(a, Log Double)]
smc2 k n p t param model =
  rmsmc
    MCMCConfig {numMCMCSteps = t, proposal = SingleSiteMH, numBurnIn = 0}
    SMCConfig {numParticles = p, numSteps = k, resampler = resampleMultinomial}
    (param >>= setup . population . smcPush (SMCConfig {numSteps = k, numParticles = n, resampler = resampleMultinomial}) . model)
