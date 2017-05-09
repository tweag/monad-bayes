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

import Numeric.CustomReal hiding (beta, gamma)
import Numeric.LogDomain
import Statistics.Distribution.Polymorphic.Class

-- | Discrete distribution on [0..n-1]
data Discrete r = Discrete (V.Vector r)

-- | Vector of probabilities for each value.
weights :: Discrete r -> V.Vector r
weights (Discrete w) = w

-- | Construct a discrete distribution normalizing the weights.
discreteDist :: (Foldable f, IsCustomReal r) => f r -> Discrete r
discreteDist ws = Discrete $ normalize $ V.fromList $ Fold.toList ws

-- | Like `discreteDist`, but weights are given in log-domain.
logDiscreteDist :: (Foldable f, IsCustomReal r) => f (LogDomain r) -> Discrete r
logDiscreteDist ws = Discrete $ V.map fromLogDomain $ normalize $ V.fromList $ Fold.toList ws

-- | Probability mass function for a discrete distribution without checking bounds.
unsafeDiscretePdf :: IsCustomReal r => V.Vector r -> Int -> LogDomain r
unsafeDiscretePdf ws i = toLogDomain (ws V.! i)

instance IsCustomReal r => Distribution (Discrete r) where
  type Domain (Discrete r)  = Int
  type RealNum (Discrete r) = r

instance IsCustomReal r => KnownSupport (Discrete r) where
  support (Discrete ws) = Range 0 (V.length ws)

instance IsCustomReal r => Parametric (Discrete r) where
  type Param (Discrete r) = V.Vector r
  param = weights
  distFromParam = discreteDist
  -- since we don't export 'Discrete' constructor, it is not possible to construct an instance
  -- with invalid parameters from outside this module
  checkParam d = Nothing

instance IsCustomReal r => Density (Discrete r) where
  unsafePdf (Discrete ws) = unsafeDiscretePdf ws
