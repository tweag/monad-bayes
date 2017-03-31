{-|
Module      : Statistics.Distribution.Polymorphic.Uniform
Description : Continuous uniform distribution
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  MultiParamTypeClasses
 #-}

module Statistics.Distribution.Polymorphic.Uniform (
  Uniform(Uniform),
  uniformDist,
  uniform
) where

import Numeric.LogDomain hiding (beta, gamma)
import Control.Monad.Bayes.Class

-- | Uniform continuous distribution.
data Uniform r = Uniform r r

-- | Construct a uniform distribution checking its parameters.
uniformDist :: (Ord r) => r -> r -> Uniform r
uniformDist a b =
  if a < b
    then Uniform a b
    else error "Uniform was given an invalid interval"

-- | PDF of a continuous uniform distribution on an interval
uniformPdf :: (Ord a, Floating a) => a -> a -> a -> LogDomain a
uniformPdf a b x =
  if a <= x && x <= b then
    recip $ toLogDomain (b - a)
  else
    0

type instance DomainType (Uniform r) = r
type instance RealNumType (Uniform r) = r

instance (Ord r, NumSpec r) => Density (Uniform r) where
  pdf (Uniform a b) = uniformPdf a b

instance Sampleable (Uniform r) Uniform where
  sample = id

-- | Sample from a uniform distribution in a probabilistic program.
uniform :: (HasCustomReal m, r ~ CustomReal m, Sampleable (Uniform r) m) => r -> r -> m r
uniform a b = sample (uniformDist a b)
