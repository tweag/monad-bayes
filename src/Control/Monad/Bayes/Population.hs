{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Control.Monad.Bayes.Population
-- Description : Representation of distributions using multiple samples
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- 'Population' turns a single sample into a collection of weighted samples.
module Control.Monad.Bayes.Population
  ( Population,
    runPopulation,
    explicitPopulation,
    fromWeightedList,
    spawn,
    resampleMultinomial,
    resampleSystematic,
    extractEvidence,
    pushEvidence,
    proper,
    evidence,
    hoist,
    collapse,
    popAvg,
  )
where

import Control.Arrow (second)
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
    ( MonadInfer,
      MonadCond,
      MonadSample(logCategorical, random, categorical),
      factor )
import Control.Monad.Bayes.Weighted
    ( Weighted, runWeighted, extractWeight, withWeight, applyWeight )
import Control.Monad.Trans ( MonadIO, MonadTrans(..) )
import Control.Monad.Trans.List ( ListT(..) )
import qualified Data.List
import qualified Data.Vector as V
import Numeric.Log ( sum, Log(ln) )
import Prelude hiding (all, sum)

-- | A collection of weighted samples, or particles.
newtype Population m a = Population (Weighted (ListT m) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadSample, MonadCond, MonadInfer)

instance MonadTrans Population where
  lift = Population . lift . lift

-- | Explicit representation of the weighted sample with weights in the log
-- domain.
runPopulation :: Population m a -> m [(a, Log Double)]
runPopulation (Population m) = runListT $ runWeighted m

-- | Explicit representation of the weighted sample.
explicitPopulation :: Functor m => Population m a -> m [(a, Double)]
explicitPopulation = fmap (map (second (exp . ln))) . runPopulation

-- | Initialize 'Population' with a concrete weighted sample.
fromWeightedList :: Monad m => m [(a, Log Double)] -> Population m a
fromWeightedList = Population . withWeight . ListT

-- | Increase the sample size by a given factor.
-- The weights are adjusted such that their sum is preserved.
-- It is therefore safe to use 'spawn' in arbitrary places in the program
-- without introducing bias.
spawn :: Monad m => Int -> Population m ()
spawn n = fromWeightedList $ pure $ replicate n ((), 1 / fromIntegral n)

resampleGeneric ::
  MonadSample m =>
  -- | resampler
  (V.Vector Double -> m [Int]) ->
  Population m a ->
  Population m a
resampleGeneric resampler m = fromWeightedList $ do
  pop <- runPopulation m
  let (xs, ps) = unzip pop
  let n = length xs
  let z = sum ps
  if z > 0
    then do
      let weights = V.fromList (map (exp . ln . (/ z)) ps)
      ancestors <- resampler weights
      let xvec = V.fromList xs
      let offsprings = map (xvec V.!) ancestors
      return $ map (,z / fromIntegral n) offsprings
    else -- if all weights are zero do not resample
      return pop

-- | Systematic resampling helper.
systematic :: Double -> V.Vector Double -> [Int]
systematic u ps = f 0 (u / fromIntegral n) 0 0 []
  where
    prob i = ps V.! i
    n = length ps
    inc = 1 / fromIntegral n
    f i _ _ _ acc | i == n = acc
    f i v j q acc =
      if v < q
        then f (i + 1) (v + inc) j q (j - 1 : acc)
        else f i v (j + 1) (q + prob j) acc

-- | Resample the population using the underlying monad and a systematic resampling scheme.
-- The total weight is preserved.
resampleSystematic ::
  (MonadSample m) =>
  Population m a ->
  Population m a
resampleSystematic = resampleGeneric (\ps -> (`systematic` ps) <$> random)

-- | Multinomial resampler.
multinomial :: MonadSample m => V.Vector Double -> m [Int]
multinomial ps = replicateM (V.length ps) (categorical ps)

-- | Resample the population using the underlying monad and a multinomial resampling scheme.
-- The total weight is preserved.
resampleMultinomial ::
  (MonadSample m) =>
  Population m a ->
  Population m a
resampleMultinomial = resampleGeneric multinomial

-- | Separate the sum of weights into the 'Weighted' transformer.
-- Weights are normalized after this operation.
extractEvidence ::
  Monad m =>
  Population m a ->
  Population (Weighted m) a
extractEvidence m = fromWeightedList $ do
  pop <- lift $ runPopulation m
  let (xs, ps) = unzip pop
  let z = sum ps
  let ws = map (if z > 0 then (/ z) else const (1 / fromIntegral (length ps))) ps
  factor z
  return $ zip xs ws

-- | Push the evidence estimator as a score to the transformed monad.
-- Weights are normalized after this operation.
pushEvidence ::
  MonadCond m =>
  Population m a ->
  Population m a
pushEvidence = hoist applyWeight . extractEvidence

-- | A properly weighted single sample, that is one picked at random according
-- to the weights, with the sum of all weights.
proper ::
  (MonadSample m) =>
  Population m a ->
  Weighted m a
proper m = do
  pop <- runPopulation $ extractEvidence m
  let (xs, ps) = unzip pop
  index <- logCategorical $ V.fromList ps
  let x = xs !! index
  return x

-- | Model evidence estimator, also known as pseudo-marginal likelihood.
evidence :: (Monad m) => Population m a -> m (Log Double)
evidence = extractWeight . runPopulation . extractEvidence

-- | Picks one point from the population and uses model evidence as a 'score'
-- in the transformed monad.
-- This way a single sample can be selected from a population without
-- introducing bias.
collapse ::
  (MonadInfer m) =>
  Population m a ->
  m a
collapse = applyWeight . proper

-- | Population average of a function, computed using unnormalized weights.
popAvg :: (Monad m) => (a -> Double) -> Population m a -> m Double
popAvg f p = do
  xs <- explicitPopulation p
  let ys = map (\(x, w) -> f x * w) xs
  let t = Data.List.sum ys
  return t

-- | Applies a transformation to the inner monad.
hoist ::
  Monad n =>
  (forall x. m x -> n x) ->
  Population m a ->
  Population n a
hoist f = fromWeightedList . f . runPopulation
