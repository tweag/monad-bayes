{-|
Module      : Control.Monad.Bayes.Inference
Description : Inference algorithms for probabilistic programs
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  ScopedTypeVariables
 #-}

module Control.Monad.Bayes.Inference (
  importance,
  importance',
  smcMultinomial,
  smcMultinomial',
  smcWithResampler,
  smcRM,
) where

import Prelude hiding (sum)

--import Debug.Trace (trace, traceM)

import qualified Data.Map as Map

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sequential as Sequential
import Control.Monad.Bayes.Population
import qualified Control.Monad.Bayes.Traced as Traced

-- | Aggregate weights of equal values.
-- The resulting list is sorted ascendingly according to values.
compact :: (Num r, Ord a) => [(a,r)] -> [(a,r)]
compact = Map.toAscList . Map.fromListWith (+)

-- | Simple importance sampling from the prior.
importance :: Monad m
           => Int -- ^ numer of samples produced
           -> Population m a -> Population m a
importance n = (spawn n >>)

-- | Multiple importance samples with post-processing that aggregates weights of equal elements.
-- It does not normalize the weights.
importance' :: (Ord a, Monad m) =>
               Int -> Population m a -> m [(a, Double)]
importance' n d = fmap compact $ explicitPopulation $ importance n d

-- | Sequential Monte Carlo from the prior with multinomial resampling.
smcMultinomial :: MonadSample m
    => Int -- ^ number of resampling points
    -> Int -- ^ number of particles
    -> Sequential (Population m) a -> Population m a
smcMultinomial = smcWithResampler resample

-- | `smcMultinomial` with post-processing like in 'importance''.
smcMultinomial' :: (Ord a, MonadSample m)
     => Int -> Int
     -> Sequential (Population m) a -> m [(a, Double)]
smcMultinomial' k n d = fmap compact $ explicitPopulation $ smcMultinomial k n d

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

-- | Like `smc`, but with a custom resampling scheme.
smcWithResampler :: Monad m =>
  (forall x. Population m x -> Population m x) -- ^ resampling function
  -> Int -- ^ number of resampling points
  -> Int -- ^ number of particles
  -> Sequential (Population m) a -> Population m a

smcWithResampler resampler k n =
  finish . composeCopies k (advance . hoist' resampler) . hoist' (spawn n >>)
  where
    hoist' = Sequential.hoistFirst

-- | Resample-move Sequential Monte Carlo.
smcRM :: MonadSample m
      => Int -- ^ number of resampling points
      -> Int -- ^ number of particles
      -> Int -- ^ number of MH transitions after each resampling
      -> Sequential (Traced.Traced (Population m)) a -- ^ model
      -> Population m a
smcRM k n t =
  Traced.marginal . finish .
    composeCopies k (advance . hoistS (composeCopies t Traced.mhStep)
                      . hoistS (hoistT resample)) .
    hoistS (hoistT (spawn n >>)) where
      hoistS = Sequential.hoistFirst
      hoistT = Traced.hoistT
