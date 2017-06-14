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

import Numeric.CustomReal hiding (beta, gamma)
import Numeric.LogDomain
import Statistics.Distribution.Polymorphic.Class

-- | Gamma distribution.
data Gamma r = Gamma r r

-- | Shape.
shape :: IsCustomReal r => Gamma r -> r
shape (Gamma s _) = s

-- | Rate.
rate :: IsCustomReal r => Gamma r -> r
rate (Gamma _ r) = r

-- | Scale.
scale :: IsCustomReal r => Gamma r -> r
scale d = recip (rate d)

-- | Construct a gamma distribution without checking its parameters.
gammaDist :: IsCustomReal r => r -> r -> Gamma r
gammaDist = Gamma

-- | PDF of gamma distribution parameterized by shape and rate assuming the argument is in support.
unsafeGammaPdf :: IsCustomReal r => r -> r -> r -> LogDomain r
unsafeGammaPdf a b x = fromLog $ a * log b + (a-1) * log x - b * x - logGamma a

instance IsCustomReal r => Distribution (Gamma r) where
  type Domain (Gamma r) = r
  type RealNum (Gamma r) = r

instance IsCustomReal r => KnownSupport (Gamma r) where
  support _ = LowerBounded 0

instance IsCustomReal r => Parametric (Gamma r) where
  type Param (Gamma r) = (r,r)
  param (Gamma s r) = (s,r)
  distFromParam = uncurry gammaDist
  checkParam (Gamma s r) =
    if s > 0 && r > 0 then
      Nothing
    else
      Just $ ParamException "Non-positive arguments to Gamma"

instance IsCustomReal r => Density (Gamma r) where
  unsafePdf d@(Gamma s r) x = case checkParam d of
    Just (ParamException e) -> error e
    Nothing -> unsafeGammaPdf s r x
