{-|
Module      : Control.Monad.Bayes.Inference.RMSMC
Description : Resample-Move Sequential Monte Carlo (RM-SMC)
Copyright   : (c) Adam Scibior, 2015-2020
License     : MIT
Maintainer  : leonhard.markert@tweag.io
Stability   : experimental
Portability : GHC

Resample-move Sequential Monte Carlo (RM-SMC) sampling.

Walter Gilks and Carlo Berzuini. 2001. Following a moving target - Monte Carlo inference for dynamic Bayesian models. /Journal of the Royal Statistical Society/ 63 (2001), 127-146. <http://www.mathcs.emory.edu/~whalen/Papers/BNs/MonteCarlo-DBNs.pdf>
-}

module Control.Monad.Bayes.Inference.RMSMC (
  rmsmc,
  rmsmcLocal,
  rmsmcBasic
) where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sequential as Seq
import Control.Monad.Bayes.Traced as Tr
import qualified Control.Monad.Bayes.Traced.Dynamic as TrDyn
import qualified Control.Monad.Bayes.Traced.Basic as TrBas
import Control.Monad.Bayes.Helpers

-- | Resample-move Sequential Monte Carlo.
rmsmc :: MonadSample m
      => Int -- ^ number of timesteps
      -> Int -- ^ number of particles
      -> Int -- ^ number of Metropolis-Hastings transitions after each resampling
      -> Sequential (Traced (Population m)) a -- ^ model
      -> Population m a
rmsmc k n t =
  marginal .
  sis (composeCopies t mhStep . hoistT resampleSystematic) k .
  hoistS (hoistT (spawn n >>))

-- | Resample-move Sequential Monte Carlo with a more efficient
-- tracing representation.
rmsmcBasic :: MonadSample m
      => Int -- ^ number of timesteps
      -> Int -- ^ number of particles
      -> Int -- ^ number of Metropolis-Hastings transitions after each resampling
      -> Sequential (TrBas.Traced (Population m)) a -- ^ model
      -> Population m a
rmsmcBasic k n t =
  TrBas.marginal .
  sis (composeCopies t TrBas.mhStep . TrBas.hoistT resampleSystematic) k .
  hoistS (TrBas.hoistT (spawn n >>))

-- | A variant of resample-move Sequential Monte Carlo
-- where only random variables since last resampling are considered
-- for rejuvenation.
rmsmcLocal :: MonadSample m
           => Int -- ^ number of timesteps
           -> Int -- ^ number of particles
           -> Int -- ^ number of Metropolis-Hastings transitions after each resampling
           -> Sequential (TrDyn.Traced (Population m)) a -- ^ model
           -> Population m a
rmsmcLocal k n t =
  TrDyn.marginal .
  sis (TrDyn.freeze . composeCopies t TrDyn.mhStep . TrDyn.hoistT resampleSystematic) k .
  hoistS (TrDyn.hoistT (spawn n >>))

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)
