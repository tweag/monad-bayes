{-|
Module      : Statistics.Distribution.Polymorphic.Types
Description : Type classes and type families for probability distributions
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  MultiParamTypeClasses
 #-}

-- Probability distributions defined polymorphically over arbitrary numeric types.
-- This module reexports for each distribution its type, a safe constructor, and a sampling helper.
-- For additional functions import specific distribution modules.
module Statistics.Distribution.Polymorphic.Class (
  DomainType,
  RealNumType,
  Density,
  pdf,
  Sampleable,
  sample
) where

import Numeric.LogDomain

-------------------------------------------------
-- Type classes and families for distributions

-- | The type corresponding to a set on which the distribution is defined.
type family DomainType d
-- | The custom real number type used by a distribution.
type family RealNumType d

-- | Probability distributions for which we can compute density.
class Density d where
  -- | Probability density function.
  -- For distributions over real numbers this is density w.r.t. the Lebesgue measure,
  -- for distributions over integers this is density w.r.t. the counting measure, aka the probability mass function.
  pdf :: d -> DomainType d -> LogDomain (RealNumType d)

-- | Type class asserting that a particular distibution can be sampled in probabilistic programs of a given type.
class Sampleable d m where
  sample :: d -> m (DomainType d)
