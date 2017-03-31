{-|
Module      : Statistics.Distribution.Polymorphic.Discrete
Description : Discrete distribution with finite support
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  MultiParamTypeClasses
 #-}

module Statistics.Distribution.Polymorphic.Discrete (
  Discrete(Discrete),
  discreteDist,
  discrete
) where

import qualified Data.Vector as V
import qualified Data.Foldable as Fold

import Numeric.LogDomain hiding (beta, gamma)
import Control.Monad.Bayes.Class

-- | Discrete distribution on [0..n-1]
data Discrete r k = Discrete {weights :: V.Vector r}

-- | Construct a discrete distribution normalizing the weights.
discreteDist :: (Foldable f, NumSpec r) => f r -> Discrete r k
discreteDist ws = Discrete $ normalize $ V.fromList $ Fold.toList ws

-- | Probability mass function for a discrete distribution.
discretePdf :: (Ord r, Floating r, Integral k) => V.Vector r -> k -> LogDomain r
discretePdf ws k = let i = fromIntegral k in
  if i >= 0 && i < length ws
    then toLogDomain (ws V.! i)
    else 0

type instance DomainType (Discrete r k)  = k
type instance RealNumType (Discrete r k) = r

instance (Ord r, Floating r, Integral k) => Density (Discrete r k) where
  pdf (Discrete ws) = discretePdf ws

instance Sampleable (Discrete r k) (Discrete r) where
  sample = id

-- | Sample from a discrete distribution in a probabilistic program.
discrete :: (Sampleable (Discrete r k) m, Foldable f, HasCustomReal m, r ~ CustomReal m, NumSpec r) => f r -> m k
discrete ws = sample (discreteDist ws)
