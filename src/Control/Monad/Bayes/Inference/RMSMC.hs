{-|
Module      : Control.Monad.Bayes.Inference.RMSMC
Description : Resample-Move Sequential Monte Carlo
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Inference.RMSMC (
  rmsmc,
  rmsmcLocal
) where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sequential as Seq
import Control.Monad.Bayes.Traced as Tr
import qualified Control.Monad.Bayes.Traced.Dynamic as TrDyn
import Control.Monad.Bayes.Helpers

-- | Resample-move Sequential Monte Carlo.
rmsmc :: MonadSample m
      => Int -- ^ number of timesteps
      -> Int -- ^ number of particles
      -> Int -- ^ number of MH transitions after each resampling
      -> Sequential (Traced (Population m)) a -- ^ model
      -> Population m a
rmsmc k n t =
  marginal .
  sis (composeCopies t mhStep . hoistT resampleSystematic) k .
  hoistS (hoistT (spawn n >>))

-- | A variant of resample-move Sequential Monte Carlo
-- where only random variables since last resampling are considered
-- for rejuvenation.
rmsmcLocal :: MonadSample m
           => Int -- ^ number of timesteps
           -> Int -- ^ number of particles
           -> Int -- ^ number of MH transitions after each resampling
           -> Sequential (TrDyn.Traced (Population m)) a -- ^ model
           -> Population m a
rmsmcLocal k n t =
  TrDyn.marginal .
  sis (TrDyn.freeze . composeCopies t TrDyn.mhStep . TrDyn.hoistT resampleSystematic) k .
  hoistS (TrDyn.hoistT (spawn n >>))

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)
