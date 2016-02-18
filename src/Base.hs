{-# LANGUAGE
  TupleSections
 #-}


module Base where

import Data.Number.LogFloat

class Monad m => MonadDist m where
    categorical :: Foldable t => t (a,Double) -> m a
    normal :: Double -> Double -> m Double
    gamma :: Double -> Double -> m Double
    beta :: Double -> Double -> m Double

class MonadDist m => MonadBayes m where
    condition :: Bool -> m ()
    condition b = if b then return () else fail "rejected"

    factor :: LogFloat -> m ()
