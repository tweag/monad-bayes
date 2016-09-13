{-|
Module      : Control.Monad.Bayes.Dist
Description : Exact representation of distributions with finite support
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

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

import Control.Monad.Bayes.LogDomain (LogDomain, fromLogDomain, toLogDomain, NumSpec)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted

-- | Representation of a discrete distribution as a list of weighted values.
-- Probabilistic computation and conditioning is performed by exact enumeration.
-- There is no automatic normalization or aggregation of weights.
--
-- The first parameter is a numeric type used to represent weights.
newtype Dist r a = Dist {unDist :: [(a, Weight r)]}

type instance CustomReal (Dist r) = r

instance Functor (Dist r) where
  fmap f = Dist . fmap (first f) . unDist

instance (Ord r, Floating r) => Applicative (Dist r) where
  pure x = Dist $ [(x,1)]
  df <*> dx = Dist [(f x, p * q) | (f,p) <- unDist df, (x,q) <- unDist dx]

instance (Ord r, Floating r) => Monad (Dist r) where
  dx >>= df = Dist [(y, p * q) | (x,p) <- unDist dx, (y,q) <- unDist (df x)]

instance (Ord r, Real r, NumSpec r) => MonadDist (Dist r) where
  discrete xs = Dist $ fmap (second (weight . toLogDomain)) $
                  normalize $ zip (map fromIntegral [0..]) xs
  normal  = error "Dist does not support continuous distributions"
  gamma   = error "Dist does not support continuous distributions"
  beta    = error "Dist does not support continuous distributions"
  uniform = error "Dist does not support continuous distributions"

instance (Ord r, Real r, NumSpec r) => MonadBayes (Dist r) where
  factor w = Dist [((), weight w)]

-- | Returns an explicit representation of a `Dist`.
-- The resulting list may contain several entries for the same value.
-- The sum of all weights is the model evidence.
toList :: Dist r a -> [(a, LogDomain r)]
toList = map (second unWeight) . unDist

-- | Same as `toList`, only weights are converted from log-domain.
explicit :: Floating r => Dist r a -> [(a,r)]
explicit = map (second fromLogDomain) . toList

-- | Returns the model evidence, that is sum of all weights.
evidence :: (Ord r, Floating r) => Dist r a -> LogDomain r
evidence = sum . map snd . toList

-- | Normalized probability mass of a specific value.
mass :: (Ord r, Floating r, Ord a) => Dist r a -> a -> r
mass d a = case lookup a (normalize (enumerate d)) of
             Just p -> p
             Nothing -> 0

-- | Aggregate weights of equal values.
-- The resulting list is sorted ascendingly according to values.
compact :: (Num r, Ord a) => [(a,r)] -> [(a,r)]
compact = Map.toAscList . Map.fromListWith (+)

-- | Like `normalize`, but additionally returns the model evidence.
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

-- | Aggregate and normalize of weights.
-- The resulting list is sorted ascendingly according to values.
--
-- > enumerate = compact . explicit
enumerate :: (Floating r, Ord a) => Dist r a -> [(a,r)]
enumerate = compact . explicit

-- | Expectation of a given function computed using unnormalized weights.
expectation :: Floating r => (a -> r) -> Dist r a -> r
expectation f p = sum $ map (\(x,w) -> f x * fromLogDomain w) $ toList p
