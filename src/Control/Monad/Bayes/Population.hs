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
    Empirical,
    runEmpirical,
    fromList,
    sampleSize,
    draw,
    Population,
    runPopulation,
    fromWeightedList,
    fromUnweighted,
    weightedSampleSize,
    spawn,
    resample,
    resampleN,
    proper,
    evidence,
    collapse,
    popAvg
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

-- | The number of samples used for approximation.
sampleSize :: Monad m => Empirical m a -> m Int
sampleSize e = do
  zs <- runEmpirical e
  return (length zs)

-- | Set the number of samples for the empirical distribution.
-- Bear in mind that invoking `draw` twice in the same computation
-- leads to multiplying the number of samples.
-- Additionally, using `draw` with different arguments in different branches
-- of a probabilistic program leads to a bias in the final sample, since
-- the branch that uses more points is over-represented.
draw :: Monad m => Int -> Empirical m ()
draw n = Empirical $ ListT $ return $ replicate n ()




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
fromUnweighted :: MonadDist m => Empirical m a -> Population m a
fromUnweighted = fromWeightedList . fmap setWeights . runEmpirical where
  setWeights xs = map (,w) xs where
    w = 1 / fromIntegral (length xs)

-- | The number of points in the weighted sample,
-- not to be confused with effective sample size.
weightedSampleSize :: MonadDist m => Population m a -> m Int
weightedSampleSize = sampleSize . runWeighted . unPopulation

-- | A variant of 'draw' for 'Population'.
-- The weights are set equal and such that they sum to 1.
-- It is therefore safe to use `spawn` in arbitrary places in the program
-- without introducing bias.
spawn :: MonadDist m => Int -> Population m ()
spawn n = fromUnweighted (draw n)

-- | A version of 'resampleN' that operates explicitly on lists.
-- Returns the empty list if model evidence is 0.
resampleNList :: MonadDist m => Int -> [(a,LogDomain (CustomReal m))] -> m [(a,LogDomain (CustomReal m))]
resampleNList n ys = do
  let (_,ws) = unzip ys
  let z = sum ws
  if z == 0 then
    return []
  else do
    offsprings <- multinomial (map (second fromLogDomain) ys) n
    let new_samples = concat [replicate k x | (x,k) <- offsprings]
    return $ map (,z / fromIntegral n) new_samples

-- | A version of 'resample' that operates explicitly on lists.
resampleList :: MonadDist m => [(a,LogDomain (CustomReal m))] -> m [(a,LogDomain (CustomReal m))]
resampleList ys = resampleNList (length ys) ys

-- | Resample the population using the underlying monad.
-- The total weight is preserved.
resample :: MonadDist m => Population m a -> Population m a
resample d = fromWeightedList $ do
  ys <- runPopulation d
  resampleList ys

-- | As 'resample', but with chosen new population size.
resampleN :: MonadDist m => Int -> Population m a -> Population m a
resampleN n d = fromWeightedList $ do
  ys <- runPopulation d
  resampleNList n ys

-- | A properly weighted single sample, that is one picked at random according
-- to the weights, with an estimator of the model evidence.
proper :: MonadDist m => Population m a -> m (a,LogDomain (CustomReal m))
proper = fmap head . (>>= resampleNList 1) . runPopulation

-- | Model evidence estimator, also known as pseudo-marginal likelihood.
evidence :: MonadDist m => Population m a -> m (LogDomain (CustomReal m))
evidence = fmap snd . proper

-- | Pick one point from the population and use model evidence as a 'factor'
-- in the transformed monad.
collapse :: (MonadBayes m) => Population m a -> m a
collapse e = do
  (x,p) <- proper e
  factor p
  return x

-- | Population average of a function, computed using unnormalized weights.
popAvg :: MonadBayes m => (a -> CustomReal m) -> Population m a -> m (CustomReal m)
popAvg f p = do
  xs <- runPopulation p
  let ys = map (\(x,w) -> f x * fromLogDomain w) xs
  return (sum ys)
