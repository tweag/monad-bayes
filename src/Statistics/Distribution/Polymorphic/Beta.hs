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
  Beta,
  alpha,
  beta,
  betaDist
) where

import Numeric.CustomReal hiding (beta, gamma)
import Numeric.LogDomain
import Statistics.Distribution.Polymorphic.Class

-- | Beta distribution.
data Beta r = Beta r r

-- | First parameter.
alpha :: IsCustomReal r => Beta r -> r
alpha (Beta a _) = a

-- | Second parameter.
beta :: IsCustomReal r => Beta r -> r
beta (Beta _ b) = b

-- | Construct a beta distribution without checking its parameters.
betaDist :: IsCustomReal r => r -> r -> Beta r
betaDist = Beta

-- | PDF of beta distribution, assuming the parameters have valid values
-- and the argument is in support.
unsafeBetaPdf :: IsCustomReal r => r -> r -> r -> LogDomain r
unsafeBetaPdf a b x = fromLog $ (a-1) * log x + (b-1) * log (1-x) - logBeta a b

instance IsCustomReal r => Distribution (Beta r) where
  type Domain (Beta r) = r
  type RealNum (Beta r) = r

instance IsCustomReal r => KnownSupport (Beta r) where
  support _ = Interval 0 1

instance IsCustomReal r => Parametric (Beta r) where
  type Param (Beta r) = (r,r)
  param (Beta a b) = (a,b)
  distFromParam = uncurry betaDist
  checkParam (Beta a b) | a <= 0 || b <= 0 = Just $ ParamException "Negative parameter to Beta"
                        | otherwise = Nothing

instance IsCustomReal r => Density (Beta r) where
  unsafePdf d@(Beta a b) x = case checkParam d of
    Just (ParamException e) -> error e
    Nothing -> unsafeBetaPdf a b x
