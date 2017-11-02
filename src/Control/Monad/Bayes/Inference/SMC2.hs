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
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Free as Free
import Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Sequential as Seq
import Control.Monad.Bayes.Traced as Tr
import Control.Monad.Bayes.Helpers

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

latent :: Monad m => S (P (P m)) a -> S (T (P (P m))) a
latent = Seq.hoist lift

pf :: MonadSample m
          => Weighted (FreeSampler (Population m)) a
          -> Weighted (FreeSampler (Population m)) [(a, Log Double)]
pf =
  hoistWF lift .
  runPopulation .
  pushWeight .
  hoistW Free.pullPopulation

pfInv :: MonadSample m
      => Weighted (FreeSampler (Population m)) [(a, Log Double)]
      -> Weighted (FreeSampler (Population m)) a
pfInv =
  hoistW pushPopulation .
  Pop.pullWeight .
  fromWeightedList .
  hoistWF (prior . proper)

smc2 :: MonadSample m
     => Int -> Int -> Int -> Int -> S (T (P (P m))) a -> P m a
smc2 k n p t =
  Pop.flatten .
  marginal . finish .
  composeCopies k (advance . hoistSM resample . hoistS (transformModel pfInv . composeCopies t mhStep . transformModel pf) . hoistSTP resample . hoistSM (lift . collapse)) .
  hoistSTP (spawn n >>) . hoistSM (spawn p >>)
