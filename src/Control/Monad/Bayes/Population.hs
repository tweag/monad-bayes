{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
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
    multinomial,
    resampleMultinomial,
    systematic,
    resampleSystematic,
    stratified,
    resampleStratified,
    extractEvidence,
    pushEvidence,
    proper,
    evidence,
    collapse,
    mapPopulation,
    normalize,
    popAvg,
    flatten,
    hoist,
  )
where

import Control.Arrow (second)
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted hiding (flatten, hoist)
import Control.Monad.Trans
import Control.Monad.Trans.List
import Data.List (unfoldr)
import qualified Data.List
import Data.Maybe (catMaybes)
import Data.Vector ((!))
import qualified Data.Vector as V
import Numeric.Log (Log, ln, sum)
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

-- | Systematic sampler.
-- Sample \(n\) values from \((0,1]\) as follows
-- \[
-- \begin{aligned}
-- u^{(1)} &\sim U\left(0, \frac{1}{n}\right] \\
-- u^{(i)} &=u^{(1)}+\frac{i-1}{n}, \quad i=2,3, \ldots, n
-- \end{aligned}
-- \]
-- and then pick integers \(m\) according to
-- \[
-- Q^{(m-1)}<u^{(n)} \leq Q^{(m)}
-- \]
-- where
-- \[
-- Q^{(m)}=\sum_{k=1}^{m} w^{(k)}
-- \]
-- and \(w^{(k)}\) are the weights. See also [Comparison of Resampling Schemes for Particle Filtering](https://arxiv.org/abs/cs/0507025).
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

-- | Stratified sampler.
--
-- Sample \(n\) values from \((0,1]\) as follows
-- \[
-- u^{(i)} \sim U\left(\frac{i-1}{n}, \frac{i}{n}\right], \quad i=1,2, \ldots, n
-- \]
-- and then pick integers \(m\) according to
-- \[
-- Q^{(m-1)}<u^{(n)} \leq Q^{(m)}
-- \]
-- where
-- \[
-- Q^{(m)}=\sum_{k=1}^{m} w^{(k)}
-- \]
-- and \(w^{(k)}\) are the weights.
--
-- The conditional variance of stratified sampling is always smaller than that of multinomial sampling and it is also unbiased - see  [Comparison of Resampling Schemes for Particle Filtering](https://arxiv.org/abs/cs/0507025).
stratified :: MonadSample m => V.Vector Double -> m [Int]
stratified weights = do
  let bigN = V.length weights
  dithers <- V.replicateM bigN (uniform 0.0 1.0)
  let positions =
        V.map (/ fromIntegral bigN) $
          V.zipWith (+) dithers (V.map fromIntegral $ V.fromList [0 .. bigN - 1])
      cumulativeSum = V.scanl (+) 0.0 weights
      coalg (i, j)
        | i < bigN =
          if (positions ! i) < (cumulativeSum ! j)
            then Just (Just j, (i + 1, j))
            else Just (Nothing, (i, j + 1))
        | otherwise =
          Nothing
  return $ map (\i -> i - 1) $ catMaybes $ unfoldr coalg (0, 0)

-- | Resample the population using the underlying monad and a stratified resampling scheme.
-- The total weight is preserved.
resampleStratified ::
  (MonadSample m) =>
  Population m a ->
  Population m a
resampleStratified = resampleGeneric stratified

-- | Multinomial sampler.  Sample from \(0, \ldots, n - 1\) \(n\)
-- times drawn at random according to the weights where \(n\) is the
-- length of vector of weights.
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

-- | Applies a random transformation to a population.
mapPopulation ::
  (Monad m) =>
  ([(a, Log Double)] -> m [(a, Log Double)]) ->
  Population m a ->
  Population m a
mapPopulation f m = fromWeightedList $ runPopulation m >>= f

-- | Normalizes the weights in the population so that their sum is 1.
-- This transformation introduces bias.
normalize :: (Monad m) => Population m a -> Population m a
normalize = hoist prior . extractEvidence

-- | Population average of a function, computed using unnormalized weights.
popAvg :: (Monad m) => (a -> Double) -> Population m a -> m Double
popAvg f p = do
  xs <- explicitPopulation p
  let ys = map (\(x, w) -> f x * w) xs
  let t = Data.List.sum ys
  return t

-- | Combine a population of populations into a single population.
flatten :: Monad m => Population (Population m) a -> Population m a
flatten m = Population $ withWeight $ ListT t
  where
    t = f <$> (runPopulation . runPopulation) m
    f d = do
      (x, p) <- d
      (y, q) <- x
      return (y, p * q)

-- | Applies a transformation to the inner monad.
hoist ::
  Monad n =>
  (forall x. m x -> n x) ->
  Population m a ->
  Population n a
hoist f = fromWeightedList . f . runPopulation
