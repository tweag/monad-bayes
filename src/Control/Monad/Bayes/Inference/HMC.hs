{-|
Module      : Control.Monad.Bayes.Inference.HMC
Description : Hamiltonian Monte Carlo
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Inference.HMC (
  nuts
) where

import qualified Numeric.MCMC.NUTS as N

import Numeric.LogDomain
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Conditional
import Control.Monad.Bayes.Deterministic
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Prior

nuts :: Int -> Int -> (forall m. MonadBayes m => m a)
     -> Trace Double -> SamplerIO [a]
nuts nAdapt n model start = fmap (map process) $ customSamplerIO $ N.nutsDualAveraging d g n nAdapt ps where
  d = toLog . fst . unsafeJointDensityGradient model
  g = snd . unsafeJointDensityGradient model
  ps = case toLists start of
    (xs, []) -> xs
    _ -> error "NUTS was initialized with a trace containing discrete variables"
  process xs = unsafeDeterministic $ prior $ unsafeConditional model $ fromLists (xs, [])
