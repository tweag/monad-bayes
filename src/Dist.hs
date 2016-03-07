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
import Importance

-- | Representation of discrete distribution as a list of weighted values.
-- Probabilistic computation and conditioning is performed by exact enumeration.
-- There is no automatic normalization or aggregation of (value,weight) pairs.
newtype Dist a = Dist {unDist :: ImportanceT [] a}
    deriving (Functor, Applicative, Monad)

instance MonadDist Dist where
    categorical d = Dist $ ImportanceT $ WriterT $ fmap (second weight) $ Fold.toList d
    normal = error "Dist does not support continuous distributions"
    gamma  = error "Dist does not support continuous distributions"
    beta   = error "Dist does not support continuous distributions"

instance MonadBayes Dist where
    factor = Dist . ImportanceT . tell . weight

-- | Returns an explicit representation of a `Dist`.
toList :: Dist a -> [(a,LogFloat)]
toList = runImportanceT . unDist

-- | Same as `toList`, only weights are converted to `Double`.
explicit :: Dist a -> [(a,Double)]
explicit = map (second fromLogFloat) . toList

-- | Aggregate weights of equal values.
compact :: Ord a => [(a,Double)] -> [(a,Double)]
compact = Map.toAscList . Map.fromListWith (+)

-- | Normalize the weights to sum to 1.
normalize :: [(a,Double)] -> [(a,Double)]
normalize xs = map (second (/ norm)) xs where
    norm = sum (map snd xs)

-- | Aggregation and normalization of weights.
enumerate :: Ord a => Dist a -> [(a,Double)]
enumerate d = simplify $ explicit d where
    simplify = normalize . compact    
    

