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
  Uniform,
  lower,
  upper,
  uniformDist
) where

import Numeric.CustomReal
import Numeric.LogDomain
import Statistics.Distribution.Polymorphic.Class

-- | Uniform continuous distribution.
data Uniform r = Uniform r r

-- | Left end of the interval.
lower :: IsCustomReal r => Uniform r -> r
lower (Uniform a _) = a

-- | Right end of the interval.
upper :: IsCustomReal r => Uniform r -> r
upper (Uniform _ b) = b

-- | Construct a uniform distribution checking its parameters.
uniformDist :: IsCustomReal r => r -> r -> Uniform r
uniformDist a b =
  if a < b
    then Uniform a b
    else error "Uniform was given an invalid interval"

-- | PDF of a continuous uniform distribution on an interval assuming the argument is in the interval.
unsafeUniformPdf :: IsCustomReal r => r -> r -> r -> LogDomain r
unsafeUniformPdf a b x = recip $ toLogDomain (b - a)

instance IsCustomReal r => Distribution (Uniform r) where
  type Domain (Uniform r) = r
  type RealNum (Uniform r) = r

instance IsCustomReal r => KnownSupport (Uniform r) where
  support (Uniform a b) = Interval a b

instance IsCustomReal r => Parametric (Uniform r) where
  type Param (Uniform r) = (r,r)
  param (Uniform a b) = (a,b)
  distFromParam = uncurry uniformDist
  checkParam (Uniform a b) =
    if a < b then
      Nothing
    else
      Just $ ParamException $ "Uniform: invalid interval"

instance IsCustomReal r => Density (Uniform r) where
  unsafePdf d@(Uniform a b) x = case checkParam d of
    Nothing -> unsafeUniformPdf a b x
    Just (ParamException e) -> error e
