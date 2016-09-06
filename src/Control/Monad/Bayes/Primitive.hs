{-# LANGUAGE
  GADTs,
  StandaloneDeriving,
  FlexibleContexts
 #-}

module Control.Monad.Bayes.Primitive (
  Primitive (Discrete, Continuous),
  PrimitiveReal (Normal, Gamma, Beta, Uniform),
  Support (Finite, Interval),
  support,
  pdf
) where

import Numeric.SpecFunctions
import Data.List

import Control.Monad.Bayes.LogDomain

data PrimitiveReal r = Normal r r | Gamma r r | Beta r r | Uniform r r
  deriving (Eq,Show)

-- | Primitive distributions for which we can compute density.
-- Here the weights of Categorical must be normalized.
data Primitive r a where
    Discrete :: (Ord r, Floating r, Real r) => [r] -> Primitive r Int
    Continuous :: (Ord r, Floating r, Real r) => PrimitiveReal r -> Primitive r r

deriving instance Eq (Primitive r a)
deriving instance Show r => Show (Primitive r a)

-- | Data type representing support of a proability distribution.
-- There is currently no distinction between open and closed intervals,
-- since it was not important in any code written so far.
data Support a where
  Finite         :: (Integral a) => [a] -> Support a
  Interval       :: (Real a, Floating a) => a -> a -> Support a
deriving instance Eq   (Support a)
deriving instance Show a => Show (Support a)

-- | Computes support of a primitive distribution.
support :: Primitive r a -> Support a
support (Discrete ps) = Finite $ findIndices (> 0) ps
support (Continuous d) = supportReal d

supportReal :: (Floating r, Real r) => PrimitiveReal r -> Support r
supportReal (Normal  _ _) = Interval (-1/0) (1/0)
supportReal (Gamma   _ _) = Interval 0 (1/0)
supportReal (Beta    _ _) = Interval 0 1
supportReal (Uniform a b) = Interval a b

-- | The probability density function.
pdf :: (Ord r, NumSpec r) => Primitive r a -> a -> LogDomain r
pdf (Discrete d) = \i -> case lookup i (zip [0..] d) of
                              Just p -> toLogDomain p
                              Nothing -> 0
pdf (Continuous d) = pdfReal d

pdfReal :: (Ord r, NumSpec r) => PrimitiveReal r -> r -> LogDomain r
pdfReal (Normal  m s) = normalPdf m s
pdfReal (Gamma   a b) = gammaPdf a b
pdfReal (Beta    a b) = betaPdf a b
pdfReal (Uniform a b) = uniformPdf a b

-- | PDF of a continuous uniform distribution on an interval
uniformPdf :: (Ord a, Floating a) => a -> a -> a -> LogDomain a
uniformPdf a b x =
  if a <= x && x <= b then
    recip $ toLogDomain (b - a)
  else
    0

-- | PDF of normal distribution parameterized by mean and stddev.
normalPdf :: Floating a => a -> a -> a -> LogDomain a
normalPdf mu sigma x =
  fromLog $ (-0.5 * log (2 * pi * sigma2)) +
  ((-((x) - (mu))^2) / (2 * sigma2))
  where
    sigma2 = sigma^2

-- | PDF of gamma distribution parameterized by shape and rate.
gammaPdf :: (Ord a, Floating a, NumSpec (LogDomain a)) => a -> a -> a -> LogDomain a
gammaPdf a b x
  | x > 0     = (fromLog $ a * log b + (a-1) * log x - b * x) - gamma (toLogDomain a)
  | otherwise = fromInteger 0

-- | PDF of beta distribution.
betaPdf :: (Ord a, Floating a, NumSpec (LogDomain a)) => a -> a -> a -> LogDomain a
betaPdf a b x
   | a <= 0 || b <= 0 = error "Negative parameter to Beta"
   | x <= 0 = fromInteger 0
   | x >= 1 = fromInteger 0
   | otherwise =
      (fromLog $ (a-1)*log x + (b-1)*log (1-x)) - beta (toLogDomain a) (toLogDomain b)
