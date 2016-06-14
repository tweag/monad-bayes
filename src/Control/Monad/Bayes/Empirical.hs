{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts
 #-}

module Control.Monad.Bayes.Empirical (
    Empirical,
    runEmpirical,
    fromList,
    sampleSize,
    draw,
    Population,
    runPopulation,
    fromWeightedList,
    fromUnweighted,
    spawn,
    resample,
    resampleN,
    proper,
    evidence,
    collapse,
                 ) where

import Prelude hiding (all)

import Control.Monad.Trans.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans.List
import Data.Number.LogFloat as LogFloat
import Data.Monoid
import qualified Data.Foldable as Fold

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted

-- | Empirical distribution represented as a list of samples.
-- Probabilistic effects are lifted from the transformed monad.
-- 'Empirical' uses 'ListT' internally, so is only a monad when the transformed
-- monad is commutative.
newtype Empirical m a = Empirical {unEmpirical :: ListT m a}
    deriving (Functor, Applicative, Monad, MonadTrans, MonadDist, MonadBayes)

-- | Explicit representation of the sample.
runEmpirical :: Functor m => Empirical m a -> m [a]
runEmpirical = runListT . unEmpirical

-- | Initialise 'Empirical' with a concrete sample.
fromList :: Monad m => m [a] -> Empirical m a
fromList = Empirical . ListT

-- | The number of samples used for approximation.
sampleSize :: Monad m => Empirical m a -> m Int
sampleSize e = do
  zs <- runEmpirical e
  return (length zs)

-- | Set the number of samples for the empirical distribution.
-- Bear in mind that invoking `spawn` twice in the same computation
-- leads to multiplying the number of samples.
draw :: Monad m => Int -> Empirical m ()
draw n = Empirical $ ListT $ return $ replicate n ()




-- | A weighted variant of 'Empirical'.
-- Conditioning is done by accumulating weights.
-- There is no automatic normalization or aggregation of weights.
newtype Population m a = Population {unPopulation :: Weighted (Empirical m) a}
  deriving (Functor, Applicative, Monad, MonadDist, MonadBayes)

instance MonadTrans Population where
  lift = Population . lift . lift

-- | Explicit representation of the weighted sample.
runPopulation :: Functor m => Population m a -> m [(a,LogFloat)]
runPopulation = runEmpirical . runWeighted . unPopulation

-- | Initialise 'Population' with a concrete weighted sample.
fromWeightedList :: Monad m => m [(a,LogFloat)] -> Population m a
fromWeightedList = Population . withWeight . fromList

-- | Lift unweighted sample by setting all weights equal.
fromUnweighted :: Monad m => Empirical m a -> Population m a
fromUnweighted = fromWeightedList . fmap setWeights . runEmpirical where
  setWeights xs = map (,w) xs where
    w = 1 / fromIntegral (length xs)

-- | A variant of 'draw' for 'Population'.
-- The weights are set equal and such that they sum to 1.
spawn :: MonadDist m => Int -> Population m ()
spawn n = fromUnweighted (draw n)

-- | A version of 'resampleN' that operates explicitly on lists.
resampleNList :: MonadDist m => Int -> [(a,LogFloat)] -> m [(a,LogFloat)]
resampleNList n ys = do
  let (xs,ws) = unzip ys
  let z = LogFloat.sum ws
  offsprings <- multinomial ys n
  let new_samples = concat [replicate k x | (x,k) <- offsprings]
  return $ map (,z / fromIntegral n) new_samples

-- | A version of 'resample' that operates explicitly on lists.
resampleList :: MonadDist m => [(a,LogFloat)] -> m [(a,LogFloat)]
resampleList ys = resampleNList (length ys) ys

-- | Resample the population using the underlying monad.
-- Model evidence estimate is preserved in total weight.
resample :: MonadDist m => Population m a -> Population m a
resample d = fromWeightedList $ do
  ys <- runPopulation d
  resampleList ys

-- | As 'resample', but with set new population size.
resampleN :: MonadDist m => Int -> Population m a -> Population m a
resampleN n d = fromWeightedList $ do
  ys <- runPopulation d
  resampleNList n ys

-- | A properly weighted single sample, that is one picked at random according
-- to the weights, with an estimator of the model evidence.
proper :: MonadDist m => Population m a -> m (a,LogFloat)
proper = fmap head . (>>= resampleNList 1) . runPopulation

-- | Model evidence estimator, also known as pseudo-marginal likelihood.
evidence :: MonadDist m => Population m a -> m LogFloat
evidence = fmap snd . proper

-- | Pick one point from the population and use model evidence as a 'factor'.
collapse :: (MonadBayes m) => Population m a -> m a
collapse e = do
  (x,p) <- proper e
  factor p
  return x
