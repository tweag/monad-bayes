{-|
Module      : Statistics.Distribution.Polymorphic.Discrete
Description : Discrete distribution with finite support
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Statistics.Distribution.Polymorphic.Discrete (
  Discrete,
  weights,
  discreteDist,
  logDiscreteDist
) where

import qualified Data.Vector as V
import qualified Data.Foldable as Fold

import Numeric.LogDomain hiding (beta, gamma)
import Statistics.Distribution.Polymorphic.Class

-- | Discrete distribution on [0..n-1]
data Discrete r k = Discrete (V.Vector r)

-- | Vector of probabilities for each value.
weights :: Discrete r k -> V.Vector r
weights (Discrete w) = w

-- | Construct a discrete distribution normalizing the weights.
discreteDist :: (Foldable f, NumSpec r) => f r -> Discrete r k
discreteDist ws = Discrete $ normalize $ V.fromList $ Fold.toList ws

-- | Like `discreteDist`, but weights are given in log-domain.
logDiscreteDist :: (Foldable f, NumSpec r, Ord r) => f (LogDomain r) -> Discrete r k
logDiscreteDist ws = Discrete $ V.map fromLogDomain $ normalize $ V.fromList $ Fold.toList ws

-- | Probability mass function for a discrete distribution.
discretePdf :: (Ord r, Floating r, Integral k) => V.Vector r -> k -> LogDomain r
discretePdf ws k = let i = fromIntegral k in
  if i >= 0 && i < length ws
    then toLogDomain (ws V.! i)
    else 0

instance (Ord r, Floating r, Integral k) => Distribution (Discrete r k) where
  type Domain (Discrete r k)  = k
  type RealNum (Discrete r k) = r

instance (Ord r, NumSpec r, Integral k) => Parametric (Discrete r k) where
  type Param (Discrete r k) = V.Vector r
  param = weights
  distFromParam = discreteDist

instance (Ord r, Floating r, Integral k) => Density (Discrete r k) where
  pdf (Discrete ws) = discretePdf ws
