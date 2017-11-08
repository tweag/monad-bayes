{-|
Module      : Control.Monad.Bayes.Inference.PMMH
Description : Particle Marginal Metropolis-Hastings
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Inference.PMMH (
  latent,
  pmmh
)  where

import Numeric.Log

import Control.Monad.Trans (lift)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Inference

latent :: Monad m => m a -> Sequential (Population (Traced m)) a
latent = lift . lift . lift

pmmh :: MonadInfer m
     => Int -- ^ number of MH steps
     -> Int -- ^ number of time steps
     -> Int -- ^ number of particles
      -> Sequential (Population (Traced m)) a -> m [[(a, Log Double)]]
pmmh t k n = mh t . runPopulation . pushEvidence . smcSystematic k n
