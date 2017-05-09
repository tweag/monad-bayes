{-|
Module      : Statistics.Distribution.Polymorphic.Class
Description : Type classes and type families for probability distributions
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Statistics.Distribution.Polymorphic.Class (
  ParamException(ParamException),
  Distribution,
  Domain,
  RealNum,
  Parametric,
  Param,
  param,
  distFromParam,
  checkParam,
  Density,
  unsafePdf,
  pdf,
  Support(Integers,Naturals,Range,RealLine,LowerBounded,UpperBounded,Interval,Euclidean),
  KnownSupport,
  support,
  inSupport
) where

import Numeric.LinearAlgebra (Vector, size)

import Numeric.LogDomain
import Numeric.CustomReal

-- | Exception indicating that a distribution was constructed with invalid parameters.
newtype ParamException = ParamException String

-- | Distribution type class.
-- It does not specify any functions, only types.
class (IsCustomReal (RealNum d)) => Distribution d where
  -- | The type corresponding to a set on which the distribution is defined.
  type Domain d
  -- | The custom real number type used by a distribution.
  type RealNum d

-- | Types that correspond to a parametric family rather than a sinle distribution.
-- A concrete distribution is obtained by setting the parameter.
class Distribution d => Parametric d where
  -- | Parameter type.
  type Param d
  -- | Getter for parameters.
  param :: d -> Param d
  -- | Creates distribution from parameters.
  distFromParam :: Param d -> d
  -- | Checks if parameters of the distribution have valid values.
  checkParam :: d -> Maybe ParamException

-- | Probability distributions for which we can compute density.
class KnownSupport d => Density d where
  -- | A variant of 'pdf' that does not check if the value is in the support of the distribution.
  -- If it is, this is equivalent to 'pdf', otherwise it produces an arbitrary result.
  unsafePdf :: d -> Domain d -> LogDomain (RealNum d)

  -- | Probability density function.
  -- For distributions over real numbers this is density w.r.t. the Lebesgue measure,
  -- for distributions over integers this is density w.r.t. the counting measure, aka the probability mass function.
  pdf :: d -> Domain d -> LogDomain (RealNum d)
  pdf d x = if x `inSupport` d then unsafePdf d x else 0

-- | Data type representing support of univariate continuous distributions.
-- We only consider single intervals, which can extend to infinity on either end.
-- There is no distinction between open and closed intervals.
data Support a where
  Integers :: Support Int
  Naturals :: Support Int
  Range :: Int -> Int -> Support Int

  RealLine :: IsCustomReal a => Support a
  LowerBounded :: IsCustomReal a => a -> Support a
  UpperBounded :: IsCustomReal a => a -> Support a
  Interval :: IsCustomReal a => a -> a -> Support a

  Euclidean :: Int -> Support (Vector Double)

-- | Distributions with known support expressible as 'Support'.
class (Distribution d) => KnownSupport d where
  -- | Support of the distribution.
  support :: d -> Support (Domain d)

-- | Checks if a value is in support of a distribution.
inSupport :: KnownSupport d => Domain d -> d -> Bool
inSupport x d = case support d of
  Integers -> True
  Naturals -> x >= 0
  Range a b -> x >= a && x < b

  RealLine -> True
  LowerBounded a -> x >= a
  UpperBounded b -> x <= b
  Interval a b -> x >= a && x <= b

  Euclidean n -> size x == n
