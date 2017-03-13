{-|
Module      : Control.Monad.Bayes
Description : Black-box inference algorithms and modelling tools
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

This module contains the classes used for probabilistic modelling and
off-the-shelf inference algorithms. It is directed at users who want to use
probabilistic programming without writing their own inference algorithms.
The type signatures are sometimes more restrictive than necessary to avoid
confusion. Advanced users should look at different modules for more permissive
variants.
-}

{-# LANGUAGE
  RankNTypes,
  TypeFamilies
 #-}

module Control.Monad.Bayes (
  module Control.Monad.Bayes.Class,
  Model,
  enumerate,
  rejection,
  importance,
  smc,
  -- smcrm,
  -- traceMH,
  pimh
) where

import Control.Arrow

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler
import qualified Control.Monad.Bayes.Enumerator as Dist
import qualified Control.Monad.Bayes.Inference as Infer

-- | A type of probabilistic models constructed with effects from
-- 'MonadDist' and 'MonadBayes' type classes.
type Model a = forall m. (MonadBayes m, CustomReal m ~ Double) => m a

processPopulation :: MonadDist m => Population m a -> m [(a, CustomReal m)]
processPopulation = fmap (map (second fromLogDomain)) . runPopulation

-- | Compute the exact posterior by accumulating all possible execution paths.
-- The resulting probabilities are aggregated (there are no repeated entries),
-- but they are not normalized - the sum of all output weights is model evience.
-- Does not work with continuous random variables - putting those in the model
-- results in an error being thrown.
enumerate :: Ord a => Model a -> [(a,Double)]
enumerate = Dist.enumerate

-- | Rejection sampling proposing from the prior.
-- Accept/reject decision is made for the whole trace, not for individual factors.
-- Factors larger than 1 are not allowed - using those throws an error.
-- In particular observing values of continuous random variables is not allowed.
rejection :: Int -- ^ number of samples accepted
          -> Model a -> IO [a]
rejection n d = sampleIO $ Infer.rejection n d

-- | Importance sampling proposing from the prior.
-- Output weights are not normalized.
importance :: Int -- ^ number of samples produced
           -> Model a -> IO [(a,Double)]
importance n m = sampleIO $ Infer.importance n m

-- | Sequential Monte Carlo.
-- Proposes from the prior and uses simple resampling after each observation.
-- If the first argument is smaller than the total number of observations,
-- there is no resampling at subsequent observations.
-- Typically the first argument should be equal to the number of
-- observations in the model.
-- Output weights are not normalized.
smc :: Int -- ^ number of resampling points
    -> Int -- ^ population size
    -> Model a -> IO [(a,Double)]
smc k n d = sampleIO $ processPopulation $ Infer.smc k n d

-- -- | Resample-move Sequential Monte Carlo using 'traceMH' for rejuvenation.
-- -- Exactly like 'smc', but additionally after each resampling the population
-- -- is rejuvanated using a number of MH transitions applied to each point
-- -- in the sample independently.
-- smcrm :: Int -- ^ number of resampling points
--       -> Int -- ^ number of MH transitions used at each resampling point
--       -> Int -- ^ population size
--       -> Model a -> IO [(a,Double)]
-- smcrm k s n d = sampleIO $ processPopulation $ Infer.smcrm k s n d
--
-- -- | Trace MH proposing modification to a single variable from the prior.
-- -- The first trace is produced from the prior and is not included in the output.
-- traceMH :: Int -- ^ number of samples produced
--         -> Model a -> IO [a]
-- traceMH n d = sampleIO $ Infer.traceMH n d

-- | Particle Indepdendent Metropolis-Hastings based on 'smc'.
-- One sample is output per each SMC run.
pimh :: Int -- ^ number of resampling points in SMC
     -> Int -- ^ population size in SMC
     -> Int -- ^ number of samples produced
     -> Model a -> IO [a]
pimh k n s d = sampleIO $ Infer.pimh k n s d
