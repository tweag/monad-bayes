{-|
Module      : Control.Monad.Bayes.Enumerator
Description : Exhaustive enumeration of discrete random variables
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Enumerator (
    Enumerator,
    toPopulation,
    hoist,
    Dist,
    logExplicit,
    explicit,
    evidence,
    mass,
    compact,
    enumerate,
    expectation
            ) where

import Control.Applicative (Applicative, pure)
import Control.Arrow (second)
import qualified Data.Map as Map
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Numeric.LogDomain (LogDomain, fromLogDomain, toLogDomain, NumSpec)
import Statistics.Distribution.Polymorphic.Discrete
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple
import qualified Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Deterministic


-- | A transformer similar to 'Population', but additionally integrates
-- discrete random variables by enumerating all execution paths.
newtype Enumerator m a = Enumerator {runEnumerator :: Pop.Population m a}
  deriving(Functor, Applicative, Monad, MonadTrans, MonadIO)

instance HasCustomReal m => HasCustomReal (Enumerator m) where
  type CustomReal (Enumerator m) = CustomReal m

instance {-# OVERLAPPING #-} (CustomReal m ~ r, MonadDist m) =>
         Sampleable (Discrete r Int) (Enumerator m) where
  sample d =
    Enumerator $ Pop.fromWeightedList $ pure $ map (second toLogDomain) $ normalize $ zip [0..] $ V.toList $ weights d

instance {-# OVERLAPPING #-} (Sampleable d m, Monad m) => Sampleable d (Enumerator m) where
  sample = lift . sample

instance (Monad m, HasCustomReal m) => Conditionable (Enumerator m) where
  factor w = Enumerator $ factor w

instance MonadDist m => MonadDist (Enumerator m)
instance MonadDist m => MonadBayes (Enumerator m)

-- | Convert 'Enumerator' to 'Population'.
toPopulation :: Enumerator m a -> Pop.Population m a
toPopulation = runEnumerator

-- | Apply a transformation to the inner monad.
hoist :: (MonadDist m, MonadDist n, CustomReal m ~ CustomReal n) =>
  (forall x. m x -> n x) -> Enumerator m a -> Enumerator n a
hoist f = Enumerator . Pop.hoist f . toPopulation

-- | A monad for discrete distributions enumerating all possible paths.
-- Throws an error if a continuous distribution is used.
type Dist r a = Enumerator (Deterministic r) a

-- | Throws an error if continuous random variables were used in 'Dist'.
ensureDiscrete :: Deterministic r a -> a
ensureDiscrete =
  fromMaybe (error "Dist: there were unhandled continuous random variables") .
  maybeDeterministic

-- | Returns the posterior as a list of weight-value pairs without any post-processing,
-- such as normalization or aggregation
logExplicit :: (Real r, NumSpec r) => Dist r a -> [(a, LogDomain r)]
logExplicit = ensureDiscrete . Pop.runPopulation . toPopulation

-- | Same as `toList`, only weights are converted from log-domain.
explicit :: (Real r, NumSpec r) => Dist r a -> [(a,r)]
explicit = map (second fromLogDomain) . logExplicit

-- | Returns the model evidence, that is sum of all weights.
evidence :: (Real r, NumSpec r) => Dist r a -> LogDomain r
evidence = ensureDiscrete . Pop.evidence . toPopulation

-- | Normalized probability mass of a specific value.
mass :: (Real r, NumSpec r, Ord a) => Dist r a -> a -> r
mass d = f where
  f a = case lookup a m of
             Just p -> p
             Nothing -> 0
  m = enumerate d

-- | Aggregate weights of equal values.
-- The resulting list is sorted ascendingly according to values.
compact :: (Num r, Ord a) => [(a,r)] -> [(a,r)]
compact = Map.toAscList . Map.fromListWith (+)

-- | Normalize the weights to sum to 1.
normalize :: Fractional p => [(a,p)] -> [(a,p)]
normalize xs = map (second (/ z)) xs where
  z = sum $ map snd xs

-- | Aggregate and normalize of weights.
-- The resulting list is sorted ascendingly according to values.
--
-- > enumerate = compact . explicit
enumerate :: (Real r, NumSpec r, Ord a) => Dist r a -> [(a,r)]
enumerate = normalize . compact . explicit

-- | Expectation of a given function computed using normalized weights.
expectation :: (Real r, NumSpec r) => (a -> r) -> Dist r a -> r
expectation f = ensureDiscrete . Pop.popAvg f . Pop.normalize . toPopulation
