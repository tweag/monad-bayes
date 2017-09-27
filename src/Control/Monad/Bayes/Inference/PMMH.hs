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
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Free as FreeSampler
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Inference

hoistW :: (Monad m)
       => (forall x. m x -> n x) -> Weighted m a -> Weighted n a
hoistW = Weighted.hoist

hoistF :: (Monad m)
       => (forall x. m x -> n x) -> FreeSampler m a -> FreeSampler n a
hoistF = FreeSampler.hoist

runPF :: MonadSample m
          => Weighted (FreeSampler (Population m)) a
          -> Weighted (FreeSampler m) [(a, Log Double)]
runPF =
  runPopulation .
  pushWeight .
  hoistW FreeSampler.pullPopulation

pmmhSetup :: MonadSample m
          => Int -> Int -> Traced (Sequential (Population m)) a -> Traced m [(a, Log Double)]
pmmhSetup k p =
  hoistMT (prior . proper . finish) . -- remove Seq and Pop layers since they're not doing anything at this point
  transformModel (hoistW (hoistF (lift . lift)) . runPF . hoistW (hoistF (smcMultinomial k p))) -- apply SMC to the marginalized variables

pmmh :: MonadSample m
     => Int
     -> Int
     -> Int
     -> Traced (Sequential (Population m)) a
     -> m [[(a, Log Double)]]
pmmh n k p =
  mh n . -- run pseudo-marginal MH on the obtained model
  pmmhSetup k p  -- augment latent space with a particle filter
