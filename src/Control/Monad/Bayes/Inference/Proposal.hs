{-|
Module      : Control.Monad.Bayes.Proposal
Description : Proposal distributions for Bayesian inference
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Inference.Proposal (
  proposeFrom,
  proposingFrom
) where

import Control.Monad.Bayes.Class

proposeFrom :: (Monad m, Density p, Density q, Sampleable q m, Conditionable m, Domain p ~ Domain q,
                RealNum p ~ RealNum q, RealNum p ~ CustomReal m)
            => q -> p -> m (Domain p)
proposeFrom q p = do
  x <- sample q
  factor $ pdf p x / pdf q x
  return x

proposingFrom :: (Monad m, Density p, Density q, Sampleable q m, Conditionable m, Domain p ~ Domain q,
                  RealNum p ~ RealNum q, RealNum p ~ CustomReal m)
              => p -> q -> m (Domain p)
proposingFrom = flip proposeFrom
