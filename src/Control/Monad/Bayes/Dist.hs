{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts,
  TypeFamilies
 #-}

module Control.Monad.Bayes.Dist (
    Dist,
    toList,
    explicit,
    evidence,
    mass,
    compact,
    normalize,
    normalizeForEvidence,
    enumerate,
    expectation
            ) where

import System.Random
import Control.Applicative (Applicative, pure, (<*>))
import Control.Arrow (first, second)
import Control.Monad (liftM, liftM2)
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Either

import Control.Monad.List
import Control.Monad.Writer

import Control.Monad.Bayes.LogDomain (LogDomain, fromLogDomain, toLogDomain)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted

-- | Representation of discrete distribution as a list of weighted values.
-- Probabilistic computation and conditioning is performed by exact enumeration.
-- There is no automatic normalization or aggregation of (value,weight) pairs.
newtype Dist a = Dist {unDist :: [(a, Weight Double)]}

type instance CustomReal Dist = Double

instance Functor Dist where
  fmap f = Dist . fmap (first f) . unDist

instance Applicative Dist where
  pure x = Dist $ [(x,1)]
  df <*> dx = Dist [(f x, p * q) | (f,p) <- unDist df, (x,q) <- unDist dx]

instance Monad Dist where
  dx >>= df = Dist [(y, p * q) | (x,p) <- unDist dx, (y,q) <- unDist (df x)]

instance MonadDist Dist where
  discrete xs = Dist $ fmap (second (weight . toLogDomain)) $
                  normalize $ zip (map fromIntegral [0..]) xs
  normal  = error "Dist does not support continuous distributions"
  gamma   = error "Dist does not support continuous distributions"
  beta    = error "Dist does not support continuous distributions"
  uniform = error "Dist does not support continuous distributions"

instance MonadBayes Dist where
  factor w = Dist [((), weight w)]

-- | Returns an explicit representation of a `Dist`.
toList :: Dist a -> [(a, LogDomain Double)]
toList = map (second unWeight) . unDist

-- | Same as `toList`, only weights are converted to `Double`.
explicit :: Dist a -> [(a,Double)]
explicit = map (second fromLogDomain) . toList

-- | Returns the model evidence, that is sum of all weights.
evidence :: Dist a -> LogDomain Double
evidence = sum . map snd . toList

-- | Probability mass of a specific value.
-- Discards model evidence.
mass :: Ord a => Dist a -> a -> Double
mass d a = case lookup a (normalize (enumerate d)) of
             Just p -> p
             Nothing -> 0

-- | Aggregate weights of equal values.
-- | The resulting list is sorted ascendingly according to values.
compact :: (Num r, Ord a) => [(a,r)] -> [(a,r)]
compact = Map.toAscList . Map.fromListWith (+)

-- | Given subprobability density, compute normalized density and model evidence
normalizeForEvidence :: (Ord p, Fractional p) => [(a,p)] -> ([(a,p)],p)
normalizeForEvidence xs =
  let
    norm = sum (map snd xs)
  in
    if norm > 0 then
      (map (second (/ norm)) xs, norm)
    else
      ([], 0)

-- | Normalize the weights to sum to 1.
normalize :: (Ord p, Fractional p) => [(a,p)] -> [(a,p)]
normalize = fst . normalizeForEvidence

-- | Aggregation and normalization of weights.
-- | The resulting list is sorted ascendingly according to values.
enumerate :: Ord a => Dist a -> [(a,Double)]
enumerate = compact . explicit

-- | Expectation of a given function, does not normalize weights.
expectation :: (a -> Double) -> Dist a -> Double
expectation f p = sum $ map (\(x,w) -> f x * fromLogDomain w) $ toList p
