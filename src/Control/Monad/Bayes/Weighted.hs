{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
module Control.Monad.Bayes.Weighted where

-- ( Weighted,
--   withWeight,
--   runWeighted,
--   extractWeight,
--   prior,
--   applyWeight,
--   hoist,
--   toBinsWeighted,
-- )

import Control.Arrow (Arrow (first))
import Control.Monad.Bayes.Class
  ( IdentityN,
    MonadCond (..),
    MonadInfer,
    MonadSample (randomGeneric),
    factor,
    normalPdf,
    random,
    runIdentityN,
    score,
  )
import Control.Monad.Bayes.Free (FreeSampler, runWith, withRandomness)
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans (MonadIO, MonadTrans (..))
import Control.Monad.Trans.State (StateT (..), mapStateT, modify)
import Data.Fixed (mod')
import Numeric.AD (grad)
import Numeric.AD.Mode.Reverse (Reverse)
import Numeric.Log (Log (Exp, ln))

-- | Execute the program using the prior distribution, while accumulating likelihood.
newtype Weighted m n a = Weighted (StateT (Log n) (m n) a)
  -- StateT is more efficient than WriterT
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Monad (m n), RealFloat n) => MonadCond n (Weighted m) where
  scoreGeneric w = Weighted (modify (* w))

instance MonadSample n m => MonadSample n (Weighted m) where
  randomGeneric = Weighted $ lift randomGeneric

instance (RealFloat n, MonadSample n m) => MonadInfer n (Weighted m)

-- | Obtain an explicit value of the likelihood for a given value.
runWeighted :: RealFloat n => Weighted m n a -> m n (a, Log n)
runWeighted (Weighted m) = runStateT m 1

-- | Compute the sample and discard the weight.
--
-- This operation introduces bias.
prior :: (Functor (m n), RealFloat n) => Weighted m n a -> m n a
prior = fmap fst . runWeighted

-- | Compute the weight and discard the sample.
extractWeight :: (Functor (m n), RealFloat n) => Weighted m n a -> m n (Log n)
extractWeight = fmap snd . runWeighted

-- | Embed a random variable with explicitly given likelihood.
--
-- > runWeighted . withWeight = id
withWeight :: (Monad (m n), RealFloat n) => m n (a, Log n) -> Weighted m n a
withWeight m = Weighted $ do
  (x, w) <- lift m
  modify (* w)
  return x

-- | Use the weight as a factor in the transformed monad.
applyWeight :: (MonadCond n m, RealFloat n) => Weighted m n a -> m n a
applyWeight m = do
  (x, w) <- runWeighted m
  scoreGeneric w
  return x

-- | Apply a transformation to the transformed monad.
hoist :: (forall x. m n x -> m' n x) -> Weighted m n a -> Weighted m' n a
hoist t (Weighted m) = Weighted $ mapStateT t m

toBinsWeighted :: Real n => n -> [(n, Log n)] -> [(n, Log n)]
toBinsWeighted binWidth = fmap (first (fst . toBin binWidth))
  where
    toBin binSize n = let lb = n `mod'` binSize in (n - lb, n - lb + binSize)
