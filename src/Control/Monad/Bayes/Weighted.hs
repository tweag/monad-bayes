{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- 'Weighted' is an instance of 'MonadCond'. Apply a 'MonadSample' transformer to
-- obtain a 'MonadInfer' that can execute probabilistic models.
module Control.Monad.Bayes.Weighted
  ( Weighted,
    withWeight,
    runWeighted,
    extractWeight,
    prior,
    flatten,
    applyWeight,
    hoist,
  )
where

import Control.Monad.Bayes.Class
  ( MonadCond (..),
    MonadInfer,
    MonadSample,
    factor,
  )
import Control.Monad.Trans (MonadIO, MonadTrans (..))
import Control.Monad.Trans.State (StateT (..), mapStateT, modify)
import Numeric.Log (Log)

-- | Execute the program using the prior distribution, while accumulating likelihood.
newtype Weighted m a = Weighted (StateT (Log Double) m a)
  -- StateT is more efficient than WriterT
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadSample)

instance Monad m => MonadCond (Weighted m) where
  score w = Weighted (modify (* w))

instance MonadSample m => MonadInfer (Weighted m)

-- | Obtain an explicit value of the likelihood for a given value.
runWeighted :: Weighted m a -> m (a, Log Double)
runWeighted (Weighted m) = runStateT m 1

-- | Compute the sample and discard the weight.
--
-- This operation introduces bias.
prior :: Functor m => Weighted m a -> m a
prior = fmap fst . runWeighted

-- | Compute the weight and discard the sample.
extractWeight :: Functor m => Weighted m a -> m (Log Double)
extractWeight = fmap snd . runWeighted

-- | Embed a random variable with explicitly given likelihood.
--
-- > runWeighted . withWeight = id
withWeight :: (Monad m) => m (a, Log Double) -> Weighted m a
withWeight m = Weighted $ do
  (x, w) <- lift m
  modify (* w)
  return x

-- | Combine weights from two different levels.
flatten :: Monad m => Weighted (Weighted m) a -> Weighted m a
flatten m = withWeight $ (\((x, p), q) -> (x, p * q)) <$> runWeighted (runWeighted m)

-- | Use the weight as a factor in the transformed monad.
applyWeight :: MonadCond m => Weighted m a -> m a
applyWeight m = do
  (x, w) <- runWeighted m
  factor w
  return x

-- | Apply a transformation to the transformed monad.
hoist :: (forall x. m x -> n x) -> Weighted m a -> Weighted n a
hoist t (Weighted m) = Weighted $ mapStateT t m
