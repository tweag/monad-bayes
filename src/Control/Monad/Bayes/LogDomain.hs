{-# LANGUAGE
  FlexibleInstances
 #-}

-- Log-domain non-negative real numbers
-- Essentially a polymorphic version of LogFloat to allow for AD
module Control.Monad.Bayes.LogDomain where

import Numeric.SpecFunctions

-- | Log-domain non-negative real numbers.
-- Essentially a polymorphic version of LogFloat to allow for AD.
newtype LogDomain a = LogDomain a
  deriving (Eq, Ord)

-- | A raw representation of a logarithm.
toLog :: LogDomain a -> a
toLog (LogDomain x) = x

-- | Inverse of `toLog`
fromLog :: a -> LogDomain a
fromLog = LogDomain

-- | Apply a function to a logarithm
liftLog :: (a -> a) -> LogDomain a -> LogDomain a
liftLog f = fromLog . f . toLog

-- | Apply a function of two arguments to two logarithms
liftLog2 :: (a -> b -> c) -> LogDomain a -> LogDomain b -> LogDomain c
liftLog2 f x y = fromLog $ f (toLog x) (toLog y)

-- | Convert to log-domain
toLogDomain :: Floating a => a -> LogDomain a
toLogDomain = fromLog . log

-- | Inverse of `log-domain`
fromLogDomain :: Floating a => LogDomain a -> a
fromLogDomain = exp . toLog

-- | Apply a function to a log-domain value by first exponentiating,
-- then applying the function, then differentiating.
mapLog :: (Floating a, Floating b) => (a -> b) -> LogDomain a -> LogDomain b
mapLog f = toLogDomain . f . fromLogDomain

instance Show a => Show (LogDomain a) where
  show = show . toLog

instance (Ord a, Floating a) => Num (LogDomain a) where
  (LogDomain x) + (LogDomain y) = if x >= y then fromLog (x + log (1 + exp(y - x))) else fromLog (y + log (1 + exp (x - y)))
  x - y = undefined
  (*) = liftLog2 (+)
  negate x = error "LogDomain does not support negation"
  abs = id
  signum x = if toLog x == -1/0 then 0 else 1
  fromInteger = toLogDomain . fromInteger

instance (Real a, Floating a) => Real (LogDomain a) where
  toRational = toRational . fromLogDomain

instance (Ord a, Floating a) => Fractional (LogDomain a) where
  (/) = liftLog2 (-)
  recip = liftLog negate
  fromRational = toLogDomain . fromRational

instance (Ord a, Floating a) => Floating (LogDomain a) where
  pi = toLogDomain pi
  exp = liftLog exp
  log = liftLog log
  sqrt = liftLog (/ 2)
  x ** y = liftLog (* fromLogDomain y) x
  logBase x y = log y / log x
  sin   = mapLog sin
  cos   = mapLog cos
  tan   = mapLog tan
  asin  = mapLog asin
  acos  = mapLog acos
  atan  = mapLog atan
  sinh  = mapLog sinh
  cosh  = mapLog cosh
  tanh  = mapLog tanh
  asinh = mapLog asinh
  acosh = mapLog acosh
  atanh = mapLog atanh

class NumSpec a where
  gamma :: a -> a
  beta  :: a -> a -> a

instance NumSpec Double where
  gamma     = exp . logGamma
  beta a b  = exp $ logBeta a b

instance NumSpec (LogDomain Double) where
  gamma     = liftLog (logGamma . exp)
  beta a b  = liftLog2 (\x y -> logBeta (exp x) (exp y)) a b
