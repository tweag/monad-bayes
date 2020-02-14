{-|
Module      : Control.Monad.Bayes.Inference.SMC2
Description : Sequential Monte Carlo squared (SMC²)
Copyright   : (c) Adam Scibior, 2015-2020
License     : MIT
Maintainer  : leonhard.markert@tweag.io
Stability   : experimental
Portability : GHC

Sequential Monte Carlo squared (SMC²) sampling.

Nicolas Chopin, Pierre E. Jacob, and Omiros Papaspiliopoulos. 2013. SMC²: an efficient algorithm for sequential analysis of state space models. /Journal of the Royal Statistical Society Series B: Statistical Methodology/ 75 (2013), 397-426. Issue 3. <https://doi.org/10.1111/j.1467-9868.2012.01046.x>
-}

module Control.Monad.Bayes.Inference.SMC2 (
  smc2
) where

import Numeric.Log
import Control.Monad.Trans

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Helpers

-- | Helper monad transformer for preprocessing the model for 'smc2'.
newtype SMC2 m a = SMC2 (S (T (P m)) a)
  deriving(Functor,Applicative,Monad)
setup :: SMC2 m a -> S (T (P m)) a
setup (SMC2 m) = m

instance MonadTrans SMC2 where
  lift = SMC2 . lift . lift . lift

instance MonadSample m => MonadSample (SMC2 m) where
  random = lift random

instance Monad m => MonadCond (SMC2 m) where
  score = SMC2 . score

instance MonadSample m => MonadInfer (SMC2 m)

-- | Sequential Monte Carlo squared.
smc2 :: MonadSample m
     => Int -- ^ number of time steps
     -> Int -- ^ number of inner particles
     -> Int -- ^ number of outer particles
     -> Int -- ^ number of MH transitions
     -> S (T (P m)) b -- ^ model parameters
     -> ( b -> S (P (SMC2 m)) a) -- ^ model
     -> P m [(a, Log Double)]
smc2 k n p t param model =
  rmsmc k p t (param >>= setup . runPopulation . smcSystematicPush k n . model)
