{-|
Module      : Statistics.Distribution.Polymorphic.Unconstrained
Description : Univariate distributions transformed to remove constraints
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Statistics.Distribution.Polymorphic.Unconstrained (
  Unconstrained,
  removeConstraints
) where

import Numeric.AD

import Numeric.LogDomain
import Statistics.Distribution.Polymorphic.Class

newtype Unconstrained d = Unconstrained d

instance KnownSupport d => Distribution (Unconstrained d) where
  type RealNum (Unconstrained d) = RealNum d
  type Domain (Unconstrained d) = Domain d

instance KnownSupport d => KnownSupport (Unconstrained d) where
  support _ = RealLine

instance (KnownSupport d, Density d) => Density (Unconstrained d) where
  pdf (Unconstrained d) x = pdf d (inverseTransformConstraints d x) * toLogDomain (inverseTransformConstraintsGrad d x)

instance (KnownSupport d, Parametric d) => Parametric (Unconstrained d) where
  type Param (Unconstrained d) = Param d
  param (Unconstrained d) = param d
  distFromParam = Unconstrained . distFromParam

removeConstraints :: KnownSupport d => d -> Unconstrained d
removeConstraints = Unconstrained

realLineTrans :: Floating a => a -> a
realLineTrans = id
realLineInvTrans :: Floating a => a -> a
realLineInvTrans = id
lowerTrans :: Floating a => a -> a -> a
lowerTrans a x = log (x - a)
lowerInvTrans :: Floating a => a -> a -> a
lowerInvTrans a y = a + exp y
upperTrans :: Floating a => a -> a -> a
upperTrans b x = log (b - x)
upperInvTrans :: Floating a => a -> a -> a
upperInvTrans b y = b - exp y
intervalTrans :: Floating a => a -> a -> a -> a
intervalTrans a b x = atanh ((x - a) / (b - a))
intervalInvTrans :: Floating a => a -> a -> a -> a
intervalInvTrans a b y = tanh y * (b - a) + a

-- | Transform the support of a distribution onto the real line.
transformConstraints :: (KnownSupport d, Floating (RealNum d)) => d -> RealNum d -> RealNum d
transformConstraints d = case support d of
  RealLine       -> realLineTrans
  LowerBounded a -> lowerTrans a
  UpperBounded b -> upperTrans b
  Interval a b   -> intervalTrans a b

-- | Inverse of 'transformConstraints'.
inverseTransformConstraints :: (KnownSupport d, Floating (RealNum d)) => d -> RealNum d -> RealNum d
inverseTransformConstraints d = case support d of
  RealLine       -> realLineInvTrans
  LowerBounded a -> lowerInvTrans a
  UpperBounded b -> upperInvTrans b
  Interval a b   -> intervalInvTrans a b

-- | First derivative of 'transformConstraints'.
transformConstraintsGrad :: (KnownSupport d, Floating (RealNum d)) => d -> RealNum d -> RealNum d
transformConstraintsGrad d = case support d of
  RealLine       -> diff $ realLineTrans
  LowerBounded a -> diff $ lowerTrans (auto a)
  UpperBounded b -> diff $ upperTrans (auto b)
  Interval a b   -> diff $ intervalTrans (auto a) (auto b)

-- | First derivative of 'inverseTransformConstraintsGrad'.
inverseTransformConstraintsGrad :: (KnownSupport d, Floating (RealNum d)) => d -> RealNum d -> RealNum d
inverseTransformConstraintsGrad d = case support d of
  RealLine       -> diff $ realLineInvTrans
  LowerBounded a -> diff $ lowerInvTrans (auto a)
  UpperBounded b -> diff $ upperInvTrans (auto b)
  Interval a b   -> diff $ intervalInvTrans (auto a) (auto b)
