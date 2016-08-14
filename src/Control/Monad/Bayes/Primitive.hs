{-# LANGUAGE
  GADTs,
  StandaloneDeriving,
  FlexibleContexts
 #-}

module Control.Monad.Bayes.Primitive where

import Data.Typeable
import Numeric.SpecFunctions
import Data.Number.LogFloat (LogFloat, logFloat, logToLogFloat)

import Control.Monad.Bayes.LogDomain

-- | Primitive distributions for which we can compute density.
-- Here the weights of Categorical must be normalized.
data Primitive a where
    Discrete :: (Typeable a, Integral a)         => [LogFloat] -> Primitive a
    Normal   :: (Typeable a, Real a, Floating a) => a -> a -> Primitive a
    Gamma    :: (Typeable a, Real a, Floating a) => a -> a -> Primitive a
    Beta     :: (Typeable a, Real a, Floating a) => a -> a -> Primitive a
    Uniform  :: (Typeable a, Real a, Floating a) => a -> a -> Primitive a

deriving instance Eq   (Primitive a)
instance Show (Primitive a) where
  show (Discrete xs) = "Discrete " ++ show xs
  show (Normal  m s) =
    "Normal "  ++ show (toRational m) ++ " " ++ show (toRational s)
  show (Gamma   a b) =
    "Gamma "   ++ show (toRational a) ++ " " ++ show (toRational b)
  show (Beta    a b) =
    "Beta "    ++ show (toRational a) ++ " " ++ show (toRational b)
  show (Uniform a b) =
    "Uniform " ++ show (toRational a) ++ " " ++ show (toRational b)

-- | The probability density function.
pdf :: Primitive a -> a -> LogFloat
pdf (Discrete d) = \i -> case lookup i (zip [0..] d) of
                              Just p -> p
                              Nothing -> logFloat 0
pdf (Normal  m s) = logToLogFloat . realToFrac . normalPdf m s
pdf (Gamma   a b) = logToLogFloat . realToFrac . gammaPdf a b
pdf (Beta    a b) = logToLogFloat . realToFrac . betaPdf a b
pdf (Uniform a b) = logToLogFloat . realToFrac . uniformPdf a b

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
