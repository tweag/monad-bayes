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

import Control.Monad.Trans (lift)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Free as FreeSampler
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Inference

hoistW :: (Monad m)
       => (forall x. m x -> n x) -> Weighted m a -> Weighted n a
hoistW = Weighted.hoist

hoistF :: (Monad m)
       => (forall x. m x -> n x) -> FreeSampler m a -> FreeSampler n a
hoistF = FreeSampler.hoist

collapse' :: MonadSample m
          => Weighted (FreeSampler (Population m)) a
          -> Weighted (FreeSampler (Population m)) a
-- TODO: switching from Weighted FreeSampler to FreeSampler Weighted in Traced might
-- help get rid of pullWeighted.
collapse' =
  hoistW (hoistF lift)  .
  Weighted.flatten .
  hoistW (FreeSampler.pullWeight . hoistF proper)

-- pmmhSetup :: MonadSample m
--           => Int -> Int -> T (S (P (W m))) a -> T (W m) a

pmmh :: MonadSample m
     => Int
     -> Int
     -> Int
     -> Traced (Sequential (Population m)) a
     -> m [a]
pmmh n k p =
  mh n . -- run pseudo-marginal MH on the obtained model
  hoistMT (prior . proper . finish) . -- remove Seq and Pop layers since they're not doing anything at this point
  transformModel (hoistW (hoistF lift) . collapse' . hoistW (hoistF (smcMultinomial k p))) . -- apply SMC to the marginalized variables
  hoistT (lift . finish) -- remove suspensions from trace distribution
