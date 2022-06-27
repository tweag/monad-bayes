{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Control.Monad.Bayes.Inference.SMC2
-- Description : Sequential Monte Carlo squared (SMC²)
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- Sequential Monte Carlo squared (SMC²) sampling.
--
-- Nicolas Chopin, Pierre E. Jacob, and Omiros Papaspiliopoulos. 2013. SMC²: an efficient algorithm for sequential analysis of state space models. /Journal of the Royal Statistical Society Series B: Statistical Methodology/ 75 (2013), 397-426. Issue 3. <https://doi.org/10.1111/j.1467-9868.2012.01046.x>
module Control.Monad.Bayes.Inference.SMC2
  ( smc2,
  )
where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Inference.RMSMC (rmsmc)
import Control.Monad.Bayes.Inference.SMC (smcSystematicPush)
import Control.Monad.Bayes.Population as Pop (Population, runPopulation)
import Control.Monad.Bayes.Sequential (Sequential)
import Control.Monad.Bayes.Traced
import Control.Monad.Trans (MonadTrans (..))
import Numeric.Log (Log)

-- | Helper monad transformer for preprocessing the model for 'smc2'.
newtype SMC2 m n a = SMC2 (Sequential (Traced (Population m)) n a)
  deriving newtype (Functor, Applicative, Monad)

setup :: SMC2 m n a -> Sequential (Traced (Population m)) n a
setup (SMC2 m) = m

-- instance RealFloat n => MonadTrans (Flip (SMC2 m)) where
--   lift = SMC2 . lift . lift . lift

instance (MonadSample n m, RealFloat n) => MonadSample n (SMC2 m) where
  randomGeneric = undefined -- lift randomGeneric

instance (Monad (m n), RealFloat n) => MonadCond n (SMC2 m) where
  scoreGeneric = undefined -- SMC2 . scoreGeneric

instance (RealFloat n, MonadSample n m) => MonadInfer n (SMC2 m)

-- | Sequential Monte Carlo squared.
smc2 ::
  (MonadSample n m, RealFloat n) =>
  -- | number of time steps
  Int ->
  -- | number of inner particles
  Int ->
  -- | number of outer particles
  Int ->
  -- | number of MH transitions
  Int ->
  -- | model parameters
  Sequential (Traced (Population m)) n b ->
  -- | model
  (b -> Sequential (Population (SMC2 m)) n a) ->
  Population m n [(a, Log n)]
smc2 k n p t param model =
  rmsmc k p t (param >>= setup . runPopulation . smcSystematicPush k n . model)
