{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
-- 'PopulationT' turns a single sample into a collection of weighted samples.
module Control.Monad.Bayes.Population
  ( PopulationT (..),
    runPopulationT,
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
    hoist,
    collapse,
    popAvg,
    withParticles,
    flatten,
    single,
  )
where

import Control.Applicative (Alternative)
import Control.Arrow (second)
import Control.Monad (MonadPlus, replicateM)
import Control.Monad.Bayes.Class
  ( MonadDistribution (categorical, logCategorical, random, uniform),
    MonadFactor,
    MonadMeasure,
    factor,
  )
import Control.Monad.Bayes.Population.Applicative qualified as Applicative
import Control.Monad.Bayes.Weighted
  ( WeightedT,
    applyWeight,
    extractWeight,
    runWeightedT,
    weightedT,
  )
import Control.Monad.Bayes.Weighted qualified as Weighted
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Data.List (unfoldr)
import Data.List qualified
import Data.Maybe (catMaybes)
import Data.Vector ((!))
import Data.Vector qualified as V
import Numeric.Log (Log, ln, sum)
import Numeric.Log qualified as Log
import Prelude hiding (all, sum)

-- | A collection of weighted samples, or particles.
--
-- This monad transformer is internally represented as a free monad,
-- which means that each layer of its computation contains a collection of weighted samples.
-- These can be flattened with 'flatten',
-- but the result is not a monad anymore.
newtype PopulationT m a = PopulationT {getPopulationT :: WeightedT (FreeT [] m) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadDistribution, MonadFactor, MonadMeasure)

instance MonadTrans PopulationT where
  lift = PopulationT . lift . lift

-- | Explicit representation of the weighted sample with weights in the log
-- domain.
runPopulationT :: (Monad m) => PopulationT m a -> m [(a, Log Double)]
runPopulationT = iterT (fmap concat . sequence) . fmap pure . runWeightedT . getPopulationT

-- | Explicit representation of the weighted sample.
explicitPopulation :: (Monad m) => PopulationT m a -> m [(a, Double)]
explicitPopulation = fmap (map (second (exp . ln))) . runPopulationT

-- | Initialize 'PopulationT' with a concrete weighted sample.
fromWeightedList :: (Monad m) => m [(a, Log Double)] -> PopulationT m a
fromWeightedList = PopulationT . weightedT . FreeT . fmap (Free . fmap pure)

-- | Increase the sample size by a given factor.
-- The weights are adjusted such that their sum is preserved.
-- It is therefore safe to use 'spawn' in arbitrary places in the program
-- without introducing bias.
spawn :: (Monad m) => Int -> PopulationT m ()
spawn n = fromWeightedList $ pure $ replicate n ((), 1 / fromIntegral n)

withParticles :: (Monad m) => Int -> PopulationT m a -> PopulationT m a
withParticles n = (spawn n >>)

resampleGeneric ::
  (MonadDistribution m) =>
  -- | resampler
  (V.Vector Double -> m [Int]) ->
  PopulationT m a ->
  PopulationT m a
resampleGeneric resampler m = fromWeightedList $ do
  pop <- runPopulationT m
  let (xs, ps) = unzip pop
  let n = length xs
  let z = Log.sum ps
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
  (MonadDistribution m) =>
  PopulationT m a ->
  PopulationT m a
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
stratified :: (MonadDistribution m) => V.Vector Double -> m [Int]
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
  (MonadDistribution m) =>
  PopulationT m a ->
  PopulationT m a
resampleStratified = resampleGeneric stratified

-- | Multinomial sampler.  Sample from \(0, \ldots, n - 1\) \(n\)
-- times drawn at random according to the weights where \(n\) is the
-- length of vector of weights.
multinomial :: (MonadDistribution m) => V.Vector Double -> m [Int]
multinomial ps = replicateM (V.length ps) (categorical ps)

-- | Resample the population using the underlying monad and a multinomial resampling scheme.
-- The total weight is preserved.
resampleMultinomial ::
  (MonadDistribution m) =>
  PopulationT m a ->
  PopulationT m a
resampleMultinomial = resampleGeneric multinomial

-- | Separate the sum of weights into the 'WeightedT' transformer.
-- Weights are normalized after this operation.
extractEvidence ::
  (Monad m) =>
  PopulationT m a ->
  PopulationT (WeightedT m) a
extractEvidence m = fromWeightedList $ do
  pop <- lift $ runPopulationT m
  let (xs, ps) = unzip pop
  let z = sum ps
  let ws = map (if z > 0 then (/ z) else const (1 / fromIntegral (length ps))) ps
  factor z
  return $ zip xs ws

-- | Push the evidence estimator as a score to the transformed monad.
-- Weights are normalized after this operation.
pushEvidence ::
  (MonadFactor m) =>
  PopulationT m a ->
  PopulationT m a
pushEvidence = single . flatten . hoist applyWeight . extractEvidence

-- | A properly weighted single sample, that is one picked at random according
-- to the weights, with the sum of all weights.
proper ::
  (MonadDistribution m) =>
  PopulationT m a ->
  WeightedT m a
proper m = do
  pop <- runPopulationT $ extractEvidence m
  let (xs, ps) = unzip pop
  index <- logCategorical $ V.fromList ps
  let x = xs !! index
  return x

-- | Model evidence estimator, also known as pseudo-marginal likelihood.
evidence :: (Monad m) => PopulationT m a -> m (Log Double)
evidence = extractWeight . runPopulationT . extractEvidence

-- | Picks one point from the population and uses model evidence as a 'score'
-- in the transformed monad.
-- This way a single sample can be selected from a population without
-- introducing bias.
collapse ::
  (MonadMeasure m) =>
  PopulationT m a ->
  m a
collapse = applyWeight . proper

-- | PopulationT average of a function, computed using unnormalized weights.
popAvg :: (Monad m) => (a -> Double) -> PopulationT m a -> m Double
popAvg f p = do
  xs <- explicitPopulation p
  let ys = map (\(x, w) -> f x * w) xs
  let t = Data.List.sum ys
  return t

-- | Applies a transformation to the inner monad.
hoist ::
  (Monad m, (Monad n)) =>
  (forall x. m x -> n x) ->
  PopulationT m a ->
  PopulationT n a
hoist f = PopulationT . Weighted.hoist (hoistFreeT f) . getPopulationT

-- | Flatten all layers of the free structure.
flatten :: (Monad m) => PopulationT m a -> Applicative.PopulationT m a
flatten = Applicative.fromWeightedList . runPopulationT

-- | Create a population from a single layer of branching computations.
--
-- Similar to 'fromWeightedListT'.
single :: (Monad m) => Applicative.PopulationT m a -> PopulationT m a
single = fromWeightedList . Applicative.runPopulationT
