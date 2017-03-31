{-|
Module      : Statistics.Distribution.Polymorphic.Gamma
Description : Gamma distribution
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  MultiParamTypeClasses
 #-}

module Statistics.Distribution.Polymorphic.Gamma (
  Gamma(Gamma),
  shape,
  rate,
  gammaDist,
  gamma
) where

import Numeric.LogDomain hiding (beta, gamma)
import Control.Monad.Bayes.Class

-- | Gamma distribution.
data Gamma r = Gamma {shape :: r, rate :: r}

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

type instance DomainType (Gamma r) = r
type instance RealNumType (Gamma r) = r

instance (Ord r, NumSpec r) => Density (Gamma r) where
  pdf (Gamma a b) = gammaPdf a b

instance Sampleable (Gamma r) Gamma where
  sample = id

-- | Sample from a gamma distribution in a probabilistic program.
gamma :: (HasCustomReal m, r ~ CustomReal m, Sampleable (Gamma r) m) => r -> r -> m r
gamma a b = sample (gammaDist a b)