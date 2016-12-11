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
  module Control.Monad.Bayes.Sampler,
  Model,
  Sample,
  enumerate,
  rejection,
  importance,
  smc,
  smcrm,
  traceMH,
  pimh
) where

import Control.Arrow

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler
import qualified Control.Monad.Bayes.Enumerator as Dist
import qualified Control.Monad.Bayes.Inference as Infer

-- | A type of probabilistic models constructed with effects from
-- 'MonadDist' and 'MonadBayes' type classes.
type Model a = forall m. (MonadBayes m, CustomReal m ~ Double) => m a
-- | A result type, which in practice means an abstract sampler.
-- Its monomorphic versions include `Sampler`, `StdSampler`, `MtSampler`, `IO`.
type Sample a = forall m. (MonadDist m, CustomReal m ~ Double) => m a

processPopulation :: MonadDist m => Population m a -> m [(a, CustomReal m)]
processPopulation = fmap (map (second fromLogDomain)) . runPopulation

-- | Compute the exact posterior by accumulating all possible execution paths.
-- The resulting probabilities are aggregated (there are no repeated entries),
-- but they are not normalized - the sum of all output weights is model evience.
enumerate :: Ord a => Model a -> [(a,Double)]
enumerate = Dist.enumerate

-- | Rejection sampling proposing from the prior.
-- Accept/reject decision is made for the whole trace, not for individual factors.
rejection :: Int -- ^ number of samples accepted
          -> Model a -> Sample [a]
rejection n d = Infer.rejection n d

-- | Importance sampling proposing from the prior.
importance :: Int -- ^ number of samples produced
           -> Model a -> Sample [(a,Double)]
importance = Infer.importance

-- | Sequential Monte Carlo.
-- Proposes from the prior and uses simple resampling after each factor.
-- If the first argument is smaller than the total number of factors,
-- there is no resampling and subsequent factors.
smc :: Int -- ^ number of resampling points
    -> Int -- ^ population size
    -> Model a -> Sample [(a,Double)]
smc k n d = processPopulation $ Infer.smc k n d

-- | Resample-move Sequential Monte Carlo using 'traceMH' for rejuvenation.
-- Exactly like 'smc', but additionally after each resampling the population
-- is rejuvanated using a number of MH transitions applied to each point
-- in the sample independently.
smcrm :: Int -- ^ number of resampling points
      -> Int -- ^ number of MH transitions used at each resampling point
      -> Int -- ^ population size
      -> Model a -> Sample [(a,Double)]
smcrm k s n d = processPopulation $ Infer.smcrm k s n d

-- | Trace MH proposing modification to a single variable from the prior.
-- The first trace is produced from the prior and is not included in the output.
traceMH :: Int -- ^ number of samples produced
        -> Model a -> Sample [a]
traceMH = Infer.traceMH

-- | Particle Indepdendent Metropolis-Hastings based on 'smc'.
-- One sample is output per each SMC run.
pimh :: Int -- ^ number of resampling points in SMC
     -> Int -- ^ population size in SMC
     -> Int -- ^ number of samples produced
     -> Model a -> Sample [a]
pimh = Infer.pimh
