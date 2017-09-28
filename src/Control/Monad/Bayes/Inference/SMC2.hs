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
  param,
  latent,
  smc2
) where

import Control.Monad.Trans

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Sequential as Seq
import Control.Monad.Bayes.Traced as Tr
import Control.Monad.Bayes.Helpers

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

param :: Monad m => T (P m) a -> S (P (T (P m))) a
param = lift . lift

latentHelper :: Monad m => P m a -> P (T (P m)) a
latentHelper = hoistP (lift . lift)

latent :: Monad m => S (P m) a -> S (P (T (P m))) a
latent = Seq.hoist latentHelper

smc2 :: MonadSample m
     => Int -> Int -> Int -> Int -> S (P (T (P m))) a -> P m a
smc2 k n p t =
  Pop.flatten .
  hoistP marginal . finish .
  composeCopies k (advance . hoistSP (composeCopies t mhStep) . hoistSPM resample . hoistS resample) .
  hoistS (hoistPM (spawn n >>) . (spawn p >>))
