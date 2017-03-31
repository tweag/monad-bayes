{-|
Module      : Statistics.Distribution.Polymorphic.Class
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

module Statistics.Distribution.Polymorphic.Class (
  Distribution,
  Domain,
  RealNum,
  Density,
  pdf
) where

import Numeric.LogDomain

-- | Distribution type class.
-- It does not specify any functions, only types.
class Distribution d where
  -- | The type corresponding to a set on which the distribution is defined.
  type Domain d
  -- | The custom real number type used by a distribution.
  type RealNum d

-- | Probability distributions for which we can compute density.
class Distribution d => Density d where
  -- | Probability density function.
  -- For distributions over real numbers this is density w.r.t. the Lebesgue measure,
  -- for distributions over integers this is density w.r.t. the counting measure, aka the probability mass function.
  pdf :: d -> Domain d -> LogDomain (RealNum d)
