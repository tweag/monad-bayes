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
module Control.Monad.Bayes.Inference.SMC where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sequential as Seq
import Control.Monad.Bayes.Weighted (runWeighted)
import Numeric.Log (Log)
import Control.Monad.Bayes.Sampler (sampleIO)
import Debug.Trace (trace)
import Control.Monad (void)
import Control.Monad.Bayes.Enumerator (enumerate, Enumerator, logExplicit)
import Control.Monad.List (ListT (ListT, runListT))

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

model :: MonadInfer m => m Bool
model = do
  x <- uniformD [True, False]
  factor (if x  then 1.0 else 0.5)
  -- factor (if x  then 1.0 else 0.5)
  -- condition x
  return x -- $ trace (show (x, "trace")) x

-- ex1 :: MonadInfer m => m
--   ([(Bool, Log Double)], Log Double)
-- ex1 :: [([(Bool, Log Double)], Double)]
ex1 = logExplicit $ runPopulation $ finish $ composeCopies 1 (advance . Seq.hoistFirst resampleMultinomial) $ Seq.hoistFirst (spawn 2 >>) model

ex2 = logExplicit $ runPopulation $ finish $ composeCopies 1 (advance . Seq.hoistFirst resampleMultinomial . advance) $ Seq.hoistFirst (spawn 2 >>) model
ex3 = enumerate $ runPopulation $ (spawn 2 >> model)

ex5 = logExplicit $ sis (uniformD [(), ()] >>) 1 $ advance $ advance $ model

ex6 = logExplicit (score 0.00001 >> uniformD [True,False])

composeCopies k f = foldr (.) id (replicate k f)


-- ex4 :: ListT Enumerator Bool
-- ex4 = enumerate $ runListT $ do
--   x <- uniformD [True, False]
--   condition x
--   return x