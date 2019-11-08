module Control.Monad.Bayes.Distributions (
  PrimitiveDistr, sample, observe,
  uniformDist, normalDist, gammaDist, betaDist,
  bernoulliDist, binomialDist, categoricalDist, geometricDist, poissonDist,
) where

import Control.Monad (when)
import Control.Monad.Bayes.Class
import Data.List
import Data.Maybe
import qualified Data.Vector as V

import Statistics.Distribution
import Statistics.Distribution.Uniform (uniformDistr)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Gamma (gammaDistr)
import Statistics.Distribution.Beta (betaDistr)
import Statistics.Distribution.Binomial (binomial)
import Statistics.Distribution.Geometric (geometric0)
import qualified Statistics.Distribution.Poisson as Poisson

import Numeric.Log

-- | Draw a value from a discrete distribution using a sequence of draws from Bernoulli.
fromPMF :: MonadSample m => (Int -> Double) -> m Int
fromPMF p = f 0 1 where
  f i r = do
    when (r < 0) $ error "fromPMF: total PMF above 1"
    let q = p i
    when (q < 0 || q > 1) $ error "fromPMF: invalid probability value"
    b <- bernoulli (q / r)
    if b then pure i else f (i+1) (r-q)

-- | Draw a value from a discrete distributions using the PMF.
discrete :: (DiscreteDistr d, MonadSample m) => d -> m Int
discrete d = fromPMF (probability d)

data PrimitiveDistr m a b = PrimitiveDistr {observer :: Maybe b -> a -> m b}

sample :: Monad m => PrimitiveDistr m a b -> a -> m b
sample d = (observer d) Nothing

observe :: Monad m => PrimitiveDistr m a b -> b -> a -> m b
observe d x = (observer d) (Just x)

scoreLogDensity :: (ContDistr d, MonadCond m) => d -> Double -> m ()
scoreLogDensity d x = score (Exp . log $ density d x)

scoreLogMass :: (DiscreteDistr d, MonadCond m) => d -> Int -> m ()
scoreLogMass d x = score (Exp . log $ probability d x)

observeDensity :: (ContDistr d, MonadCond m) => d -> Double -> m Double
observeDensity d x = scoreLogDensity d x >> return x

observeMass :: (DiscreteDistr d, MonadCond m) => d -> Int -> m Int
observeMass d x = scoreLogMass d x >> return x

uniformDist :: MonadInfer m => PrimitiveDistr m (Double, Double) Double
uniformDist = PrimitiveDistr uniformObserve where
  uniformObserve (Just x) (a, b) = observeDensity (uniformDistr a b) x
  uniformObserve Nothing (a, b)  = uniform a b

normalDist :: MonadInfer m => PrimitiveDistr m (Double, Double) Double
normalDist = PrimitiveDistr normalObserve where
  normalObserve (Just x) (loc, scale) = observeDensity (normalDistr loc scale) x
  normalObserve Nothing (loc, scale)  = normal loc scale

gammaDist :: MonadInfer m => PrimitiveDistr m (Double, Double) Double
gammaDist = PrimitiveDistr gammaObserve where
  gammaObserve (Just x) (shape, scale) =
    observeDensity (gammaDistr shape scale) x
  gammaObserve Nothing (shape, scale)  = gamma shape scale

betaDist :: MonadInfer m => PrimitiveDistr m (Double, Double) Double
betaDist = PrimitiveDistr betaObserve where
  betaObserve (Just x) (a, b) = observeDensity (betaDistr a b) x
  betaObserve Nothing (a, b)  = beta a b

bernoulliDist :: MonadInfer m => PrimitiveDistr m Double Bool
bernoulliDist = PrimitiveDistr bernoulliObserve where
  bernoulliObserve (Just b) p = debit <$> observeMass (binomial 1 p) (bit b)
  bernoulliObserve Nothing p  = bernoulli p
  bit True = 1
  bit False = 0
  debit 0 = False
  debit _ = True

binomialDist :: MonadInfer m => PrimitiveDistr m (Double, Int) Int
binomialDist = PrimitiveDistr binomialObserve where
  binomialObserve (Just i) (p, k) = observeMass (binomial k p) i
  binomialObserve Nothing (p, k)  = discrete (binomial k p)

categoricalDist :: (Eq a, MonadInfer m) => PrimitiveDistr m [(a, Log Double)] a
categoricalDist = PrimitiveDistr categoricalObserve where
  categoricalObserve (Just a) aps = do
    score (probOf a aps)
    return a
  categoricalObserve Nothing aps = do
    i <- logCategorical (V.fromList $ map snd aps)
    return (map fst aps !! i)
  probOf a aps = ((map snd aps) !!) . fromJust $ elemIndex a (map fst aps)

geometricDist :: MonadInfer m => PrimitiveDistr m Double Int
geometricDist = PrimitiveDistr geometricObserve where
  geometricObserve (Just i) p = observeMass (geometric0 p) i
  geometricObserve Nothing p  = discrete (geometric0 p)

poissonDist :: MonadInfer m => PrimitiveDistr m Double Int
poissonDist = PrimitiveDistr poissonObserve where
  poissonObserve (Just i) lambda = observeMass (Poisson.poisson lambda) i
  poissonObserve Nothing lambda  = discrete (Poisson.poisson lambda)
