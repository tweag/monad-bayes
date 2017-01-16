{-|
Module      : Control.Monad.Bayes.LogDomain
Description : Numeric types representing numbers using their logarithms
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

-- Log-domain non-negative real numbers
-- Essentially a polymorphic version of LogFloat to allow for AD
module Control.Monad.Bayes.LogDomain where

import qualified Numeric.SpecFunctions as Spec
import Data.Reflection
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian

-- | Log-domain non-negative real numbers.
-- Used to prevent underflow when computing small probabilities.
newtype LogDomain a = LogDomain a
  deriving (Eq, Ord)

-- | Convert to log-domain.
toLogDomain :: Floating a => a -> LogDomain a
toLogDomain = fromLog . log

-- | Inverse of `toLogDomain`.
--
-- > toLogDomain . fromLogDomain = id
-- > fromLogDomain . toLogDomain = id
fromLogDomain :: Floating a => LogDomain a -> a
fromLogDomain = exp . toLog

-- | Apply a function to a log-domain value by first exponentiating,
-- then applying the function, then taking the logarithm.
--
-- > mapLog f = toLogDomain . f . fromLogDomain
mapLog :: (Floating a, Floating b) => (a -> b) -> LogDomain a -> LogDomain b
mapLog f = toLogDomain . f . fromLogDomain

-- | A raw representation of a logarithm.
--
-- > toLog = log . fromLogDomain
toLog :: LogDomain a -> a
toLog (LogDomain x) = x

-- | Inverse of `toLog`.
--
-- > fromLog = toLogDomain . exp
fromLog :: a -> LogDomain a
fromLog = LogDomain

-- | Apply a function to a logarithm.
--
-- > liftLog f = fromLog . f . toLog
liftLog :: (a -> b) -> LogDomain a -> LogDomain b
liftLog f = fromLog . f . toLog

-- | Apply a function of two arguments to two logarithms.
--
-- > liftLog2 f x y = fromLog $ f (toLog x) (toLog y)
liftLog2 :: (a -> b -> c) -> LogDomain a -> LogDomain b -> LogDomain c
liftLog2 f x y = fromLog $ f (toLog x) (toLog y)

instance Show a => Show (LogDomain a) where
  show = show . toLog

instance (Ord a, Floating a) => Num (LogDomain a) where
  x + y = if y > x then y + x
                   else fromLog (toLog x + log (1 + exp (toLog y - toLog x)))
  x - y = if x >= y then fromLog (toLog x + log (1 - exp (toLog y - toLog x)))
                    else error "LogDomain: subtraction resulted in a negative value"
  (*) = liftLog2 (+)
  negate _ = error "LogDomain does not support negation"
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

-- | A class for numeric types that provide certain special functions.
class Floating a => NumSpec a where
  {-# MINIMAL (gamma | logGamma) , (beta | logBeta) #-}
  -- | Gamma function.
  gamma :: a -> a
  gamma = exp . logGamma
  -- | Beta function.
  beta  :: a -> a -> a
  beta a b = exp $ logBeta a b
  -- | Logarithm of `gamma`.
  --
  -- > logGamma = log . gamma
  logGamma :: a -> a
  logGamma = log . gamma
  -- | Logarithm of `beta`
  --
  -- > logBeta a b = log (beta a b)
  logBeta :: a -> a -> a
  logBeta a b = log $ beta a b

instance NumSpec Double where
  logGamma  = Spec.logGamma
  logBeta   = Spec.logBeta

instance (Ord a, NumSpec a) => NumSpec (LogDomain a) where
  gamma     = liftLog (logGamma . exp)
  beta a b  = liftLog2 (\x y -> logBeta (exp x) (exp y)) a b

-------------------------------------------------
-- Automatic Differentiation instances

instance (Reifies s Tape) => NumSpec (Reverse s Double) where
  logGamma = lift1 logGamma (Id . Spec.digamma . runId)
  logBeta  = lift2 logBeta (\(Id x) (Id y) -> (Id (psi x - psi (x+y)), Id (psi y - psi (x+y))))
    where psi = Spec.digamma
