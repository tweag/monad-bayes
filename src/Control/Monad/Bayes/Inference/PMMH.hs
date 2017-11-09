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
  pmmh
)  where

import Numeric.Log

import Control.Monad.Trans (lift)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Inference.SMC

pmmh :: MonadInfer m
     => Int -- ^ number of MH steps
     -> Int -- ^ number of time steps
     -> Int -- ^ number of particles
     -> Traced m b -- ^ model parameters prior
     -> (b -> Sequential (Population m) a) -- ^ model
     -> m [[(a, Log Double)]]
pmmh t k n param model =
  mh t (param >>= runPopulation . pushEvidence . Pop.hoist lift . smcSystematic k n . model)
