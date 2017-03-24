{-|
Module      : Control.Monad.Bayes.Population
Description : Representation of distributions using multiple samples
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Population (
    Population,
    runPopulation,
    explicitPopulation,
    fromWeightedList,
    spawn,
    resample,
    proper,
    evidence,
    collapse,
    mapPopulation,
    normalize,
    normalizeProper,
    popAvg,
    hoist
                 ) where

import Prelude hiding (all)

import Control.Arrow (second)
import Control.Monad.Trans
import Control.Monad.Trans.List
import Control.Monad
import Control.Applicative

import Control.Monad.Bayes.LogDomain (LogDomain, fromLogDomain)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Weighted hiding (hoist)

-- | Empirical distribution represented as a list of values.
-- Probabilistic effects are lifted from the transformed monad.
-- 'Empirical' satisfies monad laws only when the transformed
-- monad is commutative.
newtype Empirical m a = Empirical {unEmpirical :: ListT m a}
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)
instance HasCustomReal m => HasCustomReal (Empirical m) where
  type CustomReal (Empirical m) = CustomReal m
deriving instance (Sampleable d m, Monad m) => Sampleable d (Empirical m)
deriving instance (Conditionable m, Monad m) => Conditionable (Empirical m)
deriving instance MonadDist m => MonadDist (Empirical m)
deriving instance MonadBayes m => MonadBayes (Empirical m)

-- | Explicit representation of the sample.
runEmpirical :: Empirical m a -> m [a]
runEmpirical = runListT . unEmpirical

-- | Initialise 'Empirical' with a concrete sample.
fromList :: m [a] -> Empirical m a
fromList = Empirical . ListT

-- | Set the number of samples for the empirical distribution.
-- Bear in mind that invoking `draw` twice in the same computation
-- leads to multiplying the number of samples.
-- Additionally, using `draw` with different arguments in different branches
-- of a probabilistic program leads to a bias in the final sample, since
-- the branch that uses more points is over-represented.
draw :: Applicative m => Int -> Empirical m ()
draw n = fromList $ pure $ replicate n ()




-- | A population consisting of weighted samples.
-- Conditioning is done by accumulating weights.
-- There is no automatic normalization or aggregation of weights.
newtype Population m a = Population {unPopulation :: Weighted (Empirical m) a}
  deriving (Functor)

instance HasCustomReal m => HasCustomReal (Population m) where
  type CustomReal (Population m) = CustomReal m
deriving instance MonadDist m => Applicative (Population m)
deriving instance MonadDist m => Monad (Population m)
deriving instance (MonadDist m, MonadIO m) => MonadIO (Population m)
deriving instance (Sampleable d m, Monad m) => Sampleable d (Population m)
deriving instance (Monad m, HasCustomReal m) => Conditionable (Population m)
deriving instance MonadDist m => MonadDist (Population m)
deriving instance MonadDist m => MonadBayes (Population m)

instance MonadTrans Population where
  lift = Population . lift . lift

instance MonadDist m => Alternative (Population m) where
  empty = Population $ lift $ fmap undefined $ draw 0
  p1 <|> p2 = Population $ withWeight $ fromList $ do
    xs <- runPopulation p1
    ys <- runPopulation p2
    return $ xs ++ ys

instance MonadDist m => MonadPlus (Population m)

-- | Explicit representation of the weighted sample with weights in log domain.
runPopulation :: MonadDist m => Population m a -> m [(a,LogDomain (CustomReal m))]
runPopulation = runEmpirical . runWeighted . unPopulation

-- | Explicit representation of the weighted sample.
explicitPopulation :: MonadDist m => Population m a -> m [(a, CustomReal m)]
explicitPopulation = fmap (map (second fromLogDomain)) . runPopulation

-- | Initialise 'Population' with a concrete weighted sample.
fromWeightedList :: MonadDist m => m [(a,LogDomain (CustomReal m))] -> Population m a
fromWeightedList = Population . withWeight . fromList

-- | Lift unweighted sample by setting all weights equal.
fromEmpirical :: MonadDist m => Empirical m a -> Population m a
fromEmpirical = fromWeightedList . fmap setWeights . runEmpirical where
  setWeights xs = map (,w) xs where
    w = 1 / fromIntegral (length xs)

-- | Increase the sample size by a given factor.
-- The weights are adjusted such that their sum is preserved.
-- It is therefore safe to use `spawn` in arbitrary places in the program
-- without introducing bias.
spawn :: MonadDist m => Int -> Population m ()
spawn n = fromEmpirical (draw n)

-- | Resample the population using the underlying monad and a simple resampling scheme.
-- The total weight is preserved.
resample :: MonadDist m => Population m a -> Population m a
resample m = fromWeightedList $ do
  pop <- runPopulation m
  let (xs, ps) = unzip pop
  let n = length xs
  let z = sum ps
  if z > 0 then do
    offsprings <- sequenceA $ replicate n $ logCategorical pop
    return $ map (, z / fromIntegral n) offsprings
  else
    -- if all weights are zero do not resample
    return pop

-- | A properly weighted single sample, that is one picked at random according
-- to the weights, with the sum of all weights.
proper :: MonadDist m => Population m a -> m (a,LogDomain (CustomReal m))
proper m = do
  pop <- runPopulation m
  let (xs, ps) = unzip pop
  let z = sum ps
  index <- if z > 0 then
      logDiscrete ps
    else
      uniformD [0..(length xs - 1)]
  let x = xs !! index
  return (x,z)

-- | Model evidence estimator, also known as pseudo-marginal likelihood.
evidence :: MonadDist m => Population m a -> m (LogDomain (CustomReal m))
evidence = fmap (sum . map snd) . runPopulation

-- | Picks one point from the population and uses model evidence as a 'factor'
-- in the transformed monad.
-- This way a single sample can be selected from a population without
-- introducing bias.
collapse :: (MonadBayes m) => Population m a -> m a
collapse e = do
  (x,p) <- proper e
  factor p
  return x

-- | Applies a random transformation to a population.
mapPopulation :: MonadDist m => ([(a, LogDomain (CustomReal m))] -> m [(a, LogDomain (CustomReal m))]) ->
  Population m a -> Population m a
mapPopulation f m = fromWeightedList $ runPopulation m >>= f

-- | Normalizes the weights in the population so that their sum is 1.
-- This transformation introduces bias.
normalize :: MonadDist m => Population m a -> Population m a
normalize = mapPopulation norm where
    norm xs = pure $ map (second (/ z)) xs where
      z = sum $ map snd xs

-- | Normalizes the weights in the population so that their sum is 1.
-- The sum of weights is pushed as a factor to the transformed monad,
-- so bo bias is introduced.
normalizeProper :: MonadBayes m => Population m a -> Population m a
normalizeProper = mapPopulation norm where
    norm xs = factor z >> pure (map (second (/ z)) xs) where
      z = sum $ map snd xs

-- | Population average of a function, computed using unnormalized weights.
popAvg :: MonadDist m => (a -> CustomReal m) -> Population m a -> m (CustomReal m)
popAvg f p = do
  xs <- runPopulation p
  let ys = map (\(x,w) -> f x * fromLogDomain w) xs
  return (sum ys)

-- | Applies a transformation to the inner monad.
hoist :: (MonadDist m, MonadDist n, CustomReal m ~ CustomReal n) => (forall x. m x -> n x) -> Population m a -> Population n a
hoist f = fromWeightedList . f . runPopulation
