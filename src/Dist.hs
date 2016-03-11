{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts
 #-}

module Dist (
    Dist,
    toList,
    explicit,
    evidence,
    mass,
    compact,
    normalize,
    enumerate
            ) where

import System.Random
import Control.Applicative (Applicative, pure, (<*>))
import Control.Arrow (first, second)
import Control.Monad (liftM, liftM2)
import Data.Number.LogFloat (LogFloat, fromLogFloat, logFloat)
import qualified Data.Number.LogFloat as LogFloat
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Either

import Control.Monad.List
import Control.Monad.Writer

import Base
import Weighted

-- | Representation of discrete distribution as a list of weighted values.
-- Probabilistic computation and conditioning is performed by exact enumeration.
-- There is no automatic normalization or aggregation of (value,weight) pairs.
newtype Dist a = Dist {unDist :: WeightedT [] a}
    deriving (Functor, Applicative, Monad)

instance MonadDist Dist where
    categorical d = Dist $ WeightedT $ WriterT $ fmap (second weight) $
                    normalize $ Fold.toList d
    normal = error "Dist does not support continuous distributions"
    gamma  = error "Dist does not support continuous distributions"
    beta   = error "Dist does not support continuous distributions"

instance MonadBayes Dist where
    factor = Dist . WeightedT . tell . weight

-- | Returns an explicit representation of a `Dist`.
toList :: Dist a -> [(a,LogFloat)]
toList = runWeightedT . unDist

-- | Same as `toList`, only weights are converted to `Double`.
explicit :: Dist a -> [(a,Double)]
explicit = map (second fromLogFloat) . toList

-- | Returns the model evidence, that is sum of all weights.
evidence :: Dist a -> LogFloat
evidence = LogFloat.sum . map snd . toList

-- | Probability mass of a specific value.
mass :: Ord a => Dist a -> a -> Double
mass d a = case lookup a (enumerate d) of Just p -> p
                                          Nothing -> 0

-- | Aggregate weights of equal values.
-- | The resulting list is sorted ascendingly according to values.
compact :: Ord a => [(a,Double)] -> [(a,Double)]
compact = Map.toAscList . Map.fromListWith (+)

-- | Normalize the weights to sum to 1.
normalize :: (Fractional p) => [(a,p)] -> [(a,p)]
normalize xs = map (second (/ norm)) xs where
    norm = sum (map snd xs)

-- | Aggregation and normalization of weights.
-- | The resulting list is sorted ascendingly according to values.
enumerate :: Ord a => Dist a -> [(a,Double)]
enumerate d = simplify $ explicit d where
    simplify = normalize . compact
