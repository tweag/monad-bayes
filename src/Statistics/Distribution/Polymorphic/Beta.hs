{-|
Module      : Statistics.Distribution.Polymorphic.Beta
Description : Beta distribution
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  MultiParamTypeClasses
 #-}

module Statistics.Distribution.Polymorphic.Beta (
  Beta(Beta),
  betaDist,
  beta
) where

import Numeric.LogDomain hiding (beta, gamma)
import Control.Monad.Bayes.Class

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

type instance DomainType (Beta r) = r
type instance RealNumType (Beta r) = r

instance (Ord r, NumSpec r) => Density (Beta r) where
  pdf (Beta a b) = betaPdf a b

instance Sampleable (Beta r) Beta where
  sample = id

-- | Sample from a beta distribution in a probabilistic program.
beta :: (HasCustomReal m, r ~ CustomReal m, Sampleable (Beta r) m) => r -> r -> m r
beta a b = sample (betaDist a b)
