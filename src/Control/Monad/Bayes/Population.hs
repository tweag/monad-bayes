{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    onlyBelowEffectiveSampleSize,
    effectiveSampleSize,
    extractEvidence,
    pushEvidence,
    proper,
    evidence,
    hoist,
    collapse,
    popAvg,
    withParticles,
  )
where

import Control.Applicative (Alternative)
import Control.Arrow (second)
import Control.Monad (forM, replicateM)
import Control.Monad.Bayes.Class
  ( MonadDistribution (..),
    MonadFactor (..),
    MonadMeasure,
    MonadUniformRange (..),
    factor,
  )
import Control.Monad.Bayes.Weighted
  ( WeightedT,
    applyWeight,
    extractWeight,
    runWeightedT,
    weightedT,
  )
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Compose
import Data.List (unfoldr)
import Data.List qualified
import Data.Maybe (catMaybes)
import Data.Vector ((!))
import Data.Vector qualified as V
import Numeric.Log (Log, ln, sum)
import Numeric.Log qualified as Log
import Prelude hiding (all, sum)

-- | The old-fashioned, broken list transformer, adding a list/nondeterminism/choice effect.
--  It is not a valid monad transformer, but it is a valid 'Applicative'.
newtype ListT m a = ListT {getListT :: Compose m [] a}
  deriving newtype (Functor, Applicative, Alternative)

listT :: m [a] -> ListT m a
listT = ListT . Compose

runListT :: ListT m a -> m [a]
runListT = getCompose . getListT

-- | This monad instance is _unlawful_,
-- it is only by accident and careful construction that it can be used here.
instance (Monad m) => Monad (ListT m) where
  ma >>= f = ListT $ Compose $ do
    as <- runListT ma
    fmap concat $ forM as $ runListT . f

instance MonadTrans ListT where
  lift = ListT . Compose . fmap pure

instance (MonadIO m) => MonadIO (ListT m) where
  liftIO = lift . liftIO

instance (MonadDistribution m) => MonadDistribution (ListT m) where
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical

instance (MonadUniformRange m) => MonadUniformRange (ListT m) where
  uniformR l u = lift $ uniformR l u

instance (MonadFactor m) => MonadFactor (ListT m) where
  score = lift . score

instance (MonadMeasure m) => MonadMeasure (ListT m)

-- | A collection of weighted samples, or particles.
newtype PopulationT m a = PopulationT {getPopulationT :: WeightedT (ListT m) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadDistribution, MonadFactor, MonadMeasure, MonadUniformRange)

instance MonadTrans PopulationT where
  lift = PopulationT . lift . lift

-- | Explicit representation of the weighted sample with weights in the log
-- domain.
runPopulationT :: PopulationT m a -> m [(a, Log Double)]
runPopulationT = runListT . runWeightedT . getPopulationT

-- | Explicit representation of the weighted sample.
explicitPopulation :: (Functor m) => PopulationT m a -> m [(a, Double)]
explicitPopulation = fmap (map (second (exp . ln))) . runPopulationT

-- | Initialize 'PopulationT' with a concrete weighted sample.
fromWeightedList :: (Monad m) => m [(a, Log Double)] -> PopulationT m a
fromWeightedList = PopulationT . weightedT . listT

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

-- ** Effective sample size

-- | Only use the given resampler when the effective sample size is below a certain threshold.
--
-- See 'withEffectiveSampleSize'.
onlyBelowEffectiveSampleSize ::
  (MonadDistribution m) =>
  -- | The threshold under which the effective sample size must fall before the resampler is used.
  --   For example, this may be half of the number of particles.
  Double ->
  -- | The resampler to user under the threshold
  (forall n. (MonadDistribution n) => PopulationT n a -> PopulationT n a) ->
  -- | The new resampler
  (PopulationT m a -> PopulationT m a)
onlyBelowEffectiveSampleSize threshold resampler pop = fromWeightedList $ do
  (as, ess) <- withEffectiveSampleSize pop
  if ess < threshold then runPopulationT $ resampler $ fromWeightedList $ pure as else return as

-- | Compute the effective sample size of a population from the weights.
--
--  See https://en.wikipedia.org/wiki/Design_effect#Effective_sample_size
effectiveSampleSize :: (Functor m) => PopulationT m a -> m Double
effectiveSampleSize = fmap snd . withEffectiveSampleSize

-- | Compute the effective sample size alongside the samples themselves.
--
-- The advantage over 'effectiveSampleSize' is that the samples need not be created a second time.
withEffectiveSampleSize :: (Functor m) => PopulationT m a -> m ([(a, Log Double)], Double)
withEffectiveSampleSize = fmap (\as -> (as, effectiveSampleSizeKish $ (exp . ln . snd) <$> as)) . runPopulationT
  where
    effectiveSampleSizeKish :: [Double] -> Double
    effectiveSampleSizeKish weights = square (Data.List.sum weights) / Data.List.sum (square <$> weights)
    square :: Double -> Double
    square x = x * x

-- ** Utility functions

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
pushEvidence = hoist applyWeight . extractEvidence

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
  (Monad n) =>
  (forall x. m x -> n x) ->
  PopulationT m a ->
  PopulationT n a
hoist f = fromWeightedList . f . runPopulationT
