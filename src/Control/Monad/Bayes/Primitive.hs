{-# LANGUAGE
  GADTs,
  StandaloneDeriving,
  FlexibleContexts
 #-}

module Control.Monad.Bayes.Primitive where

import Data.Typeable
import Numeric.SpecFunctions
--import Data.Number.LogDomain Double (LogDomain Double, LogDomain Double, logToLogDomain Double)

import Control.Monad.Bayes.LogDomain

-- | Primitive distributions for which we can compute density.
-- Here the weights of Categorical must be normalized.
data Primitive r a where
    Discrete :: [r] -> Primitive r Int
    Normal   :: r -> r -> Primitive r r
    Gamma    :: r -> r -> Primitive r r
    Beta     :: r -> r -> Primitive r r
    Uniform  :: r -> r -> Primitive r r

deriving instance Eq r => Eq   (Primitive r a)
instance Show r => Show (Primitive r a) where
  show (Discrete xs) = "Discrete " ++ show xs
  show (Normal  m s) =
    "Normal "  ++ show m ++ " " ++ show s
  show (Gamma   a b) =
    "Gamma "   ++ show a ++ " " ++ show b
  show (Beta    a b) =
    "Beta "    ++ show a ++ " " ++ show b
  show (Uniform a b) =
    "Uniform " ++ show a ++ " " ++ show b

-- | The probability density function.
pdf :: (Ord r, Floating r, NumSpec (LogDomain r)) => Primitive r a -> a -> LogDomain r
pdf (Discrete d) = \i -> case lookup i (zip [0..] d) of
                              Just p -> toLogDomain p
                              Nothing -> 0
pdf (Normal  m s) = normalPdf m s
pdf (Gamma   a b) = gammaPdf a b
pdf (Beta    a b) = betaPdf a b
pdf (Uniform a b) = uniformPdf a b

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
