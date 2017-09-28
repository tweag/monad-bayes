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

import Control.Monad.Trans

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Sequential as Seq
import Control.Monad.Bayes.Traced as Tr
import Control.Monad.Bayes.Helpers

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

latent :: Monad m => S (P (P m)) a -> T (S (P (P m))) a
latent = lift

smc2 :: MonadSample m
     => Int -> Int -> Int -> Int -> S (T (P (P m))) a -> P m a
smc2 k n p t =
  Pop.flatten .
  marginal . finish .
  composeCopies k (advance . hoistSM resample . hoistS (composeCopies t mhStep) . hoistSTP resample . hoistSM (lift . collapse)) .
  hoistSTP (spawn n >>) . hoistSM (spawn p >>)
