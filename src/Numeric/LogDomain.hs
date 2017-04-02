{-|
Module      : Numeric.LogDomain
Description : Numeric types representing numbers using their logarithms
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

-- Log-domain non-negative real numbers.
-- Essentially a polymorphic version of LogFloat to allow for AD.
-- Unlike LogFloat, we do not attempt to fix any problems with standard floating-point numbers,
-- in particular NaN is a legal value in LogDomain and will usually propagate silently.
-- In most cases +Inf behaves in the usual way, the only exception being that addition involving +Inf results in NaN.
-- Since LogDomain can not represent negative numbers, -Inf is not a legal value.
-- Zeroes are represented internally as -Inf but this should be transpartent to the user.
-- Note that if the base type does not have -Inf then LogDomain can not represent zeros.
module Numeric.LogDomain (
  LogDomain,
  toLogDomain,
  fromLogDomain,
  mapLogDomain,
  toLog,
  fromLog,
  liftLog,
  liftLog2,
  NumSpec,
  gamma,
  beta,
  logGamma,
  logBeta,
  sum,
  normalize
) where

import Prelude hiding (sum)
import qualified Data.Foldable as Fold
import qualified Data.Traversable as Traverse
import qualified Numeric.SpecFunctions as Spec
import Data.Reflection
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian

-- | Log-domain non-negative real numbers.
-- Used to prevent under- and overflow.
newtype LogDomain a = LogDomain a
  deriving (Eq, Ord, Show, Read)

-- | Convert to log-domain.
-- Modulo floating-point errors this is an isomorphism with respect to any numeric operations.
-- The inverse is given by `fromLogDomain`.
toLogDomain :: (Ord a, Floating a) => a -> LogDomain a
toLogDomain x =
  if x < 0 then
    error "LogDomain: attempted to convert a negative number to LogDomain"
  else
    fromLog $ log x

-- | Inverse of `toLogDomain`.
fromLogDomain :: Floating a => LogDomain a -> a
fromLogDomain = exp . toLog

-- | Apply a function to a log-domain value by first exponentiating,
-- then applying the function, then taking the logarithm.
--
-- > mapLogDomain f = toLogDomain . f . fromLogDomain
mapLogDomain :: (Ord b, Floating a, Floating b) => (a -> b) -> LogDomain a -> LogDomain b
mapLogDomain f = toLogDomain . f . fromLogDomain

-- | A raw representation of a logarithm.
-- Modulo floating-point errors the following holds.
-- > toLog = log . fromLogDomain
toLog :: LogDomain a -> a
toLog (LogDomain x) = x

-- | Inverse of `toLog`.
-- Modulo floating-point errors the following holds.
-- > fromLog = toLogDomain . exp
fromLog :: a -> LogDomain a
fromLog = LogDomain

-- | Apply a function to a logarithm.
-- Avoids unnecessarily applying `log` `.` `exp`.
--
-- > liftLog f = fromLog . f . toLog
liftLog :: (a -> b) -> LogDomain a -> LogDomain b
liftLog f = fromLog . f . toLog

-- | Apply a function of two arguments to two logarithms.
--
-- > liftLog2 f x y = fromLog $ f (toLog x) (toLog y)
liftLog2 :: (a -> b -> c) -> LogDomain a -> LogDomain b -> LogDomain c
liftLog2 f x y = fromLog $ f (toLog x) (toLog y)

instance (Ord a, Floating a) => Num (LogDomain a) where
  x + y =
    -- if one of inputs is +inf, the result is NaN
    let m = max x y in
      if m == 0 then
        -- can't just return 0 here as one of inputs can be NaN
        toLogDomain $ fromLogDomain x + fromLogDomain y
      else
        -- first divide by m to avoid under- and overflow
        m * (toLogDomain $ fromLogDomain (x / m) + fromLogDomain (y / m))
  x - y =
    if y > x then
      error "LogDomain: subtraction resulted in a negative value"
    else
      if x == 0 then
        -- unlike just returning zero, this ensures that NaNs propagate through
        negate y
      else
        -- numerically stable subtraction
        x * (toLogDomain $ 1 - fromLogDomain (y / x))
  (*) = liftLog2 (+)
  negate x =
    if x > 0 then
      error "LogDomain: attempted to negate a positive number"
    else
      -- unlike just returning zero, this ensures that NaNs propagate through
      toLogDomain $ negate $ fromLogDomain x
  abs = id
  signum = toLogDomain . signum . fromLogDomain -- not the fastest way, but preserves NaNs
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
  log = liftLog $ \x ->
    if x < 0 then
      error "LogDomain: attempted to take log of a value smaller than 1"
    else
      log x
  sqrt = liftLog (/ 2)
  x ** y = liftLog (* fromLogDomain y) x
  logBase x y =
    if x < 1 then
      -- if both arguments are smaller than 1 then the result may be positive even if natural logs are not
      log (recip y) / log (recip x)
    else
      log y / log x
  sin   = mapLogDomain sin
  cos   = mapLogDomain cos
  tan   = mapLogDomain tan
  asin  = mapLogDomain asin
  acos  = mapLogDomain acos
  atan  = mapLogDomain atan
  sinh  = mapLogDomain sinh
  cosh  = mapLogDomain cosh
  tanh  = mapLogDomain tanh
  asinh = mapLogDomain asinh
  acosh = mapLogDomain acosh
  atanh = mapLogDomain atanh

-------------------------------------------------
-- NumSpec class

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

  -- | Efficient sum.
  sum :: Foldable f => f a -> a
  sum = Fold.sum

  -- | Divides all elements by their sum.
  -- Throws an error if the sum of all elements is zero or undefined.
  -- May return NaN's if the sum is infinite.
  normalize :: Traversable f => f a -> f a
  normalize t = Traverse.fmapDefault (/ z) t where
    z = sum t
    -- specific instances may provide extra checks to avoid division by 0 etc.
    -- we can't do that here without restricting the type of a

instance NumSpec Double where
  logGamma  = Spec.logGamma
  logBeta   = Spec.logBeta

  -- just like default, but with extra checks
  normalize t = Traverse.fmapDefault (/ z) t where
    z = let z' = sum t in
          if isInfinite z' then error "Normalize: infinite sum" else
            if z' > 0 then z' else error "Normalize: bad weights"

instance (Ord a, NumSpec a) => NumSpec (LogDomain a) where
  gamma     = liftLog (logGamma . exp)
  beta a b  = liftLog2 (\x y -> logBeta (exp x) (exp y)) a b

  -- sum is made more efficient by only converting all elements to linear domain once,
  -- then summing them in linear domain and taking a logarithm of the result
  -- to prevent overflow all numbers are first divided by their maximum,
  -- and the final result is mulitplied by it
  sum t = let z = Fold.maximum t in
    if z == 0 then
      -- unlike just returning zero, this ensures that NaNs propagate through
      toLogDomain (Fold.foldl' (\x y -> x + fromLogDomain y) 0 t)
    else
      z * toLogDomain (Fold.foldl' (\x y -> x + fromLogDomain (y / z)) 0 t)

  -- just like default, but with extra checks
  normalize t = Traverse.fmapDefault (/ z) t where
    z = let z' = sum t in
          if z' > 0 then z' else error "Normalize: bad weights"

-------------------------------------------------
-- Automatic Differentiation instances

instance (Reifies s Tape) => NumSpec (Reverse s Double) where
  logGamma = lift1 logGamma (Id . Spec.digamma . runId)
  logBeta  = lift2 logBeta (\(Id x) (Id y) -> (Id (psi x - psi (x+y)), Id (psi y - psi (x+y))))
    where psi = Spec.digamma
  -- TODO: can we check the weights in normalize here?
