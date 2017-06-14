{-|
Module      : Statistics.Distribution.Polymorphic.Normal
Description : Univariate normal distribution
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Statistics.Distribution.Polymorphic.Normal (
  Normal,
  mean,
  stddev,
  variance,
  precision,
  normalDist
) where

import Numeric.CustomReal
import Numeric.LogDomain
import Statistics.Distribution.Polymorphic.Class

-- | Normal distribution.
data Normal r = Normal r r

-- | Mean value.
mean :: IsCustomReal r => Normal r -> r
mean (Normal m _) = m

-- | Standard deviation.
stddev :: IsCustomReal r => Normal r -> r
stddev (Normal _ s) = s

-- | Variance.
variance :: IsCustomReal r => Normal r -> r
variance d = let s = stddev d in s * s

-- | Precision.
precision :: IsCustomReal r => Normal r -> r
precision d = recip (variance d)

-- | Create a normal distribution checking its parameters.
normalDist :: IsCustomReal r => r -> r -> Normal r
normalDist mu sigma
  | sigma <= 0 = error "Normal was given non-positive standard deviation"
  | otherwise  = Normal mu sigma

-- | PDF of a normal distribution parameterized by mean and stddev.
unsafeNormalPdf :: IsCustomReal r => r -> r -> r -> LogDomain r
unsafeNormalPdf mu sigma x =
  fromLog $ (-0.5 * log (2 * pi * sq sigma)) + ((- sq (x - mu)) / (2 * sq sigma)) where
                sq y = y ^ (2 :: Int)

instance IsCustomReal r => Distribution (Normal r) where
  type Domain (Normal r) = r
  type RealNum (Normal r) = r

instance IsCustomReal r => KnownSupport (Normal r) where
  support _ = RealLine

instance IsCustomReal r => Parametric (Normal r) where
  type Param (Normal r) = (r,r)
  param (Normal m s) = (m,s)
  distFromParam = uncurry normalDist
  checkParam (Normal _ s) =
    if s > 0 then
      Nothing
    else
      Just $ ParamException "Normal: standard deviation was not positive"

instance IsCustomReal r => Density (Normal r) where
  unsafePdf d@(Normal m s) x = case checkParam d of
    Nothing -> unsafeNormalPdf m s x
    Just (ParamException e) -> error e
