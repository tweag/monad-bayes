{-|
Module      : Statistics.Distribution.Polymorphic.Beta
Description : Beta distribution
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Statistics.Distribution.Polymorphic.Beta (
  Beta(Beta),
  betaDist
) where

import Numeric.LogDomain hiding (beta, gamma)
import Statistics.Distribution.Polymorphic.Class

-- | Beta distribution.
data Beta r = Beta r r

-- | Construct a beta distribution checking its parameters.
betaDist :: (Ord r, Floating r) => r -> r -> Beta r
betaDist a b =
  if a > 0 && b > 0
    then Beta a b
    else error "Non-positive arguments to Beta"

-- | PDF of beta distribution.
betaPdf :: (Ord a, NumSpec a) => a -> a -> a -> LogDomain a
betaPdf a b x
   | a <= 0 || b <= 0 = error "Negative parameter to Beta"
   | x <= 0 = 0
   | x >= 1 = 0
   | otherwise = fromLog $ (a-1) * log x + (b-1) * log (1-x) - logBeta a b

instance Distribution (Beta r) where
  type Domain (Beta r) = r
  type RealNum (Beta r) = r

instance (Ord r, Floating r) => Parametric (Beta r) where
  type Param (Beta r) = (r,r)
  param (Beta a b) = (a,b)
  distFromParam = uncurry betaDist

instance (Ord r, NumSpec r) => Density (Beta r) where
  pdf (Beta a b) = betaPdf a b
