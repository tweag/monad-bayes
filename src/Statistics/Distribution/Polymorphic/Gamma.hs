{-|
Module      : Statistics.Distribution.Polymorphic.Gamma
Description : Gamma distribution
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Statistics.Distribution.Polymorphic.Gamma (
  Gamma,
  shape,
  rate,
  scale,
  gammaDist
) where

import Numeric.LogDomain hiding (beta, gamma)
import Statistics.Distribution.Polymorphic.Class

-- | Gamma distribution.
data Gamma r = Gamma r r

-- | Shape.
shape :: Gamma r -> r
shape (Gamma s _) = s

-- | Rate.
rate :: Gamma r -> r
rate (Gamma _ r) = r

-- | Scale.
scale :: Fractional r => Gamma r -> r
scale d = recip (rate d)

-- | Construct a gamma distribution checking its parameters.
gammaDist :: (Ord r, Floating r) => r -> r -> Gamma r
gammaDist a b =
  if a > 0 && b > 0
    then Gamma a b
    else error "Non-positive arguments to Gamma"

-- | PDF of gamma distribution parameterized by shape and rate.
gammaPdf :: (Ord a, NumSpec a) => a -> a -> a -> LogDomain a
gammaPdf a b x
  | x > 0     = fromLog $ a * log b + (a-1) * log x - b * x - logGamma a
  | otherwise = 0

instance (Ord r, Floating r) => Distribution (Gamma r) where
  type Domain (Gamma r) = r
  type RealNum (Gamma r) = r

instance (Ord r, Floating r) => KnownSupport (Gamma r) where
  support _ = LowerBounded 0

instance (Ord r, Floating r) => Parametric (Gamma r) where
  type Param (Gamma r) = (r,r)
  param (Gamma s r) = (s,r)
  distFromParam = uncurry gammaDist

instance (Ord r, NumSpec r) => Density (Gamma r) where
  pdf (Gamma a b) = gammaPdf a b
