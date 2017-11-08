{-|
Module      : Control.Monad.Bayes.Inference.SMC2
Description : Sequential Monte Carlo ^2
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Inference.SMC2 (
  latent,
  smc2
) where

import Numeric.Log
import Control.Monad.Trans

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Helpers

latent :: Monad m => m a -> S (P (S (T (P m)))) a
latent = lift . lift . lift . lift . lift

smc2 :: MonadSample m
     => Int -- ^ number of time steps
     -> Int -- ^ number of inner particles
     -> Int -- ^ number of outer particles
     -> Int -- ^ number of MH transitions
     -> S (P (S (T (P m)))) a -> P m [(a, Log Double)]
smc2 k n p t = rmsmc k p t . runPopulation . smcSystematicPush k n
