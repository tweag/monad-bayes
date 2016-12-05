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
    fromWeightedList,
    fromEmpirical,
    spawn,
    resample,
    proper,
    evidence,
    collapse,
    mapPopulation,
    normalize,
    popAvg,
    hoist
                 ) where

import Prelude hiding (all)

import Control.Arrow (second)
import Control.Monad.Trans.Class
import Control.Monad.Trans.List

import Control.Monad.Bayes.LogDomain (LogDomain, fromLogDomain)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted

-- | Empirical distribution represented as a list of values.
-- Probabilistic effects are lifted from the transformed monad.
-- 'Empirical' satisfies monad laws only when the transformed
-- monad is commutative.
newtype Empirical m a = Empirical {unEmpirical :: ListT m a}
    deriving (Functor, Applicative, Monad, MonadTrans)
type instance CustomReal (Empirical m) = CustomReal m
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




-- | A weighted variant of 'Empirical'.
-- Conditioning is done by accumulating weights.
-- There is no automatic normalization or aggregation of weights.
newtype Population m a = Population {unPopulation :: Weighted (Empirical m) a}
  deriving (Functor)

type instance CustomReal (Population m) = CustomReal m
deriving instance MonadDist m => Applicative (Population m)
deriving instance MonadDist m => Monad (Population m)
deriving instance MonadDist m => MonadDist (Population m)
deriving instance MonadDist m => MonadBayes (Population m)

instance MonadTrans Population where
  lift = Population . lift . lift

-- | Explicit representation of the weighted sample.
runPopulation :: MonadDist m => Population m a -> m [(a,LogDomain (CustomReal m))]
runPopulation = runEmpirical . runWeighted . unPopulation

-- | Initialise 'Population' with a concrete weighted sample.
fromWeightedList :: MonadDist m => m [(a,LogDomain (CustomReal m))] -> Population m a
fromWeightedList = Population . withWeight . fromList

-- | Lift unweighted sample by setting all weights equal.
fromEmpirical :: MonadDist m => Empirical m a -> Population m a
fromEmpirical = fromWeightedList . fmap setWeights . runEmpirical where
  setWeights xs = map (,w) xs where
    w = 1 / fromIntegral (length xs)

-- | A variant of 'draw' for 'Population'.
-- The weights are set equal and such that they sum to 1.
-- It is therefore safe to use `spawn` in arbitrary places in the program
-- without introducing bias.
spawn :: MonadDist m => Int -> Population m ()
spawn n = fromEmpirical (draw n)

-- | Resample the population using the underlying monad.
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
-- to the weights, with an estimator of the model evidence.
proper :: MonadDist m => Population m a -> m (a,LogDomain (CustomReal m))
proper m = do
  pop <- runPopulation m
  let (xs, ps) = unzip pop
  let z = sum ps
  index <- if z > 0 then
      logDiscrete ps
    else
      uniformD [0..(length xs)]
  let x = xs !! index
  return (x,z)

-- | Model evidence estimator, also known as pseudo-marginal likelihood.
evidence :: MonadDist m => Population m a -> m (LogDomain (CustomReal m))
evidence = fmap (sum . map snd) . runPopulation

-- | Pick one point from the population and use model evidence as a 'factor'
-- in the transformed monad.
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
normalize :: MonadDist m => Population m a -> Population m a
normalize = mapPopulation norm where
    norm xs = pure $ map (second (/ z)) xs where
      z = sum $ map snd xs

-- | Population average of a function, computed using unnormalized weights.
popAvg :: MonadBayes m => (a -> CustomReal m) -> Population m a -> m (CustomReal m)
popAvg f p = do
  xs <- runPopulation p
  let ys = map (\(x,w) -> f x * fromLogDomain w) xs
  return (sum ys)

-- | Applies a transformation to the inner monad.
hoist :: (MonadDist m, MonadDist n, CustomReal m ~ CustomReal n) => (forall x. m x -> n x) -> Population m a -> Population n a
hoist f = fromWeightedList . f . runPopulation
