{-|
Module      : Statistics.Distribution.Polymorphic.Uniform
Description : Continuous uniform distribution
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Statistics.Distribution.Polymorphic.Uniform (
  Uniform(Uniform),
  uniformDist
) where

import Numeric.LogDomain hiding (beta, gamma)
import Statistics.Distribution.Polymorphic.Class

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

instance Distribution (Uniform r) where
  type Domain (Uniform r) = r
  type RealNum (Uniform r) = r

instance (Ord r, Floating r) => Parametric (Uniform r) where
  type Param (Uniform r) = (r,r)
  param (Uniform a b) = (a,b)
  distFromParam = uncurry uniformDist

instance (Ord r, NumSpec r) => Density (Uniform r) where
  pdf (Uniform a b) = uniformPdf a b
