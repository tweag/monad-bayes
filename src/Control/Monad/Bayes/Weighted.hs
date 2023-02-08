{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Control.Monad.Bayes.Weighted
-- Description : Probability monad accumulating the likelihood
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- 'WeightedT' is an instance of 'MonadFactor'. Apply a 'MonadDistribution' transformer to
-- obtain a 'MonadMeasure' that can execute probabilistic models.
module Control.Monad.Bayes.Weighted
  ( WeightedT,
    weightedT,
    extractWeight,
    unweighted,
    applyWeight,
    hoist,
    runWeightedT,
  )
where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Bayes.Class
  ( MonadDistribution,
    MonadFactor (..),
    MonadMeasure,
    factor,
  )
import Control.Monad.State (MonadIO, MonadTrans, StateT (..), lift, mapStateT, modify)
import Numeric.Log (Log)

-- | Execute the program using the prior distribution, while accumulating likelihood.
newtype WeightedT m a = WeightedT (StateT (Log Double) m a)
  -- StateT is more efficient than WriterT
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadTrans, MonadDistribution)

instance (Monad m) => MonadFactor (WeightedT m) where
  score w = WeightedT (modify (* w))

instance (MonadDistribution m) => MonadMeasure (WeightedT m)

-- | Obtain an explicit value of the likelihood for a given value.
runWeightedT :: WeightedT m a -> m (a, Log Double)
runWeightedT (WeightedT m) = runStateT m 1

-- | Compute the sample and discard the weight.
--
-- This operation introduces bias.
unweighted :: (Functor m) => WeightedT m a -> m a
unweighted = fmap fst . runWeightedT

-- | Compute the weight and discard the sample.
extractWeight :: (Functor m) => WeightedT m a -> m (Log Double)
extractWeight = fmap snd . runWeightedT

-- | Embed a random variable with explicitly given likelihood.
--
-- > runWeightedT . weightedT = id
weightedT :: (Monad m) => m (a, Log Double) -> WeightedT m a
weightedT m = WeightedT $ do
  (x, w) <- lift m
  modify (* w)
  return x

-- | Use the weight as a factor in the transformed monad.
applyWeight :: (MonadFactor m) => WeightedT m a -> m a
applyWeight m = do
  (x, w) <- runWeightedT m
  factor w
  return x

-- | Apply a transformation to the transformed monad.
hoist :: (forall x. m x -> n x) -> WeightedT m a -> WeightedT n a
hoist t (WeightedT m) = WeightedT $ mapStateT t m
