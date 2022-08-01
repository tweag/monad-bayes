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
-- 'Weighted' is an instance of 'MonadCond'. Apply a 'MonadSample' transformer to
-- obtain a 'MonadInfer' that can execute probabilistic models.
module Control.Monad.Bayes.Weighted
  ( Weighted,
    withWeight,
    weighted,
    extractWeight,
    unweighted,
    applyWeight,
    hoist,
    runWeighted,
  )
where

import Control.Arrow (Arrow (first))
import Control.Monad.Bayes.Class
  ( MonadCond (..),
    MonadInfer,
    MonadSample,
    factor,
  )
import Control.Monad.Trans (MonadIO, MonadTrans (..))
import Control.Monad.Trans.State (StateT (..), mapStateT, modify)
import Data.Fixed (mod')
import Numeric.Log (Log)

-- | Execute the program using the prior distribution, while accumulating likelihood.
newtype Weighted m a = Weighted (StateT (Log Double) m a)
  -- StateT is more efficient than WriterT
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadSample)

instance Monad m => MonadCond (Weighted m) where
  score w = Weighted (modify (* w))

instance MonadSample m => MonadInfer (Weighted m)

-- | Obtain an explicit value of the likelihood for a given value.
weighted, runWeighted :: Weighted m a -> m (a, Log Double)
weighted (Weighted m) = runStateT m 1
runWeighted = weighted

-- | Compute the sample and discard the weight.
--
-- This operation introduces bias.
unweighted :: Functor m => Weighted m a -> m a
unweighted = fmap fst . weighted

-- | Compute the weight and discard the sample.
extractWeight :: Functor m => Weighted m a -> m (Log Double)
extractWeight = fmap snd . weighted

-- | Embed a random variable with explicitly given likelihood.
--
-- > weighted . withWeight = id
withWeight :: (Monad m) => m (a, Log Double) -> Weighted m a
withWeight m = Weighted $ do
  (x, w) <- lift m
  modify (* w)
  return x

-- | Use the weight as a factor in the transformed monad.
applyWeight :: MonadCond m => Weighted m a -> m a
applyWeight m = do
  (x, w) <- weighted m
  factor w
  return x

-- | Apply a transformation to the transformed monad.
hoist :: (forall x. m x -> n x) -> Weighted m a -> Weighted n a
hoist t (Weighted m) = Weighted $ mapStateT t m
