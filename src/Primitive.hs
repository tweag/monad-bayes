{-# LANGUAGE
  GADTs
 #-}

module Primitive where

import Numeric.SpecFunctions
import Data.Number.LogFloat (LogFloat, logFloat, logToLogFloat)
import Data.Typeable

-- | Primitive distributions for which we can compute density.
-- Here the weights of Categorical must be normalized.
data Primitive a where
    Categorical :: (Eq a, Typeable a) => [(a,LogFloat)] -> Primitive a
    Normal :: Double -> Double -> Primitive Double
    Gamma :: Double -> Double -> Primitive Double
    Beta :: Double -> Double -> Primitive Double

instance Eq (Primitive a) where
  Categorical xs == Categorical ys = xs == ys
  Normal m s     == Normal m' s'   = m == m' && s == s'
  Gamma  a b     == Gamma  a' b'   = a == a' && b == b'
  Beta   a b     == Beta   a' b'   = a == a' && b == b'
  _              == _              = False

-- | The probability density function.
pdf :: Primitive a -> a -> LogFloat
pdf (Categorical d) = \x -> case lookup x d of
                              Just p -> p
                              Nothing -> logFloat 0
pdf (Normal m s) = normalPdf m s
pdf (Gamma  a b) = gammaPdf a b
pdf (Beta   a b) = betaPdf a b


-- | PDF of normal distribution parameterized by mean and stddev.
normalPdf :: (Real a) => a -> a -> a -> LogFloat
normalPdf mu sigma x =
  logToLogFloat $ (-0.5 * log (2 * pi * sigma2)) +
  ((-((realToFrac x) - (realToFrac mu))^2) / (2 * sigma2))
  where
    sigma2 = realToFrac sigma^2

-- | PDF of gamma distribution parameterized by shape and rate.
gammaPdf :: Double -> Double -> Double -> LogFloat
gammaPdf a b x
  | x > 0     = logToLogFloat $ a * log b - logGamma a + (a-1) * log x - b * x
  | otherwise = logFloat 0

-- | PDF of beta distribution.
betaPdf :: Double -> Double -> Double -> LogFloat
betaPdf a b x
   | a <= 0 || b <= 0 = error "Negative parameter to Beta"
   | x <= 0 = logFloat 0
   | x >= 1 = logFloat 0
   | otherwise = logToLogFloat $ (a-1)*log x + (b-1)*log (1-x) - logBeta a b

