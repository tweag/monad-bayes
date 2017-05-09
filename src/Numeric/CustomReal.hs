{-|
Module      : Numeric.CustomReal
Description : Abstraction for types representing real numbers extending standard classes
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Numeric.CustomReal (
  IsCustomReal,
  toCustomReal,
  fromCustomReal,
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

import Numeric.LogDomain

-- | Numeric types that can be treated as real numbers.
class (Floating a, Ord a) => IsCustomReal a where
  {-# MINIMAL toCustomReal, fromCustomReal, (gamma | logGamma) , (beta | logBeta) #-}
  toCustomReal :: Double -> a
  fromCustomReal :: a -> Double

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

instance IsCustomReal Double where
  toCustomReal = id
  fromCustomReal = id
  logGamma  = Spec.logGamma
  logBeta   = Spec.logBeta

instance (Reifies s Tape) => IsCustomReal (Reverse s Double) where
  toCustomReal = auto . toCustomReal
  fromCustomReal = fromCustomReal . primal
  logGamma = lift1 logGamma (Id . Spec.digamma . runId)
  logBeta  = lift2 logBeta (\(Id x) (Id y) -> (Id (psi x - psi (x+y)), Id (psi y - psi (x+y))))
    where psi = Spec.digamma

instance IsCustomReal r => IsCustomReal (LogDomain r) where
  toCustomReal = toLogDomain . toCustomReal
  fromCustomReal = fromCustomReal . fromLogDomain
  gamma     = liftLog (logGamma . exp)
  beta a b  = liftLog2 (\x y -> logBeta (exp x) (exp y)) a b

-- -------------------------------------------------
-- -- NumSpec class
--
-- -- | A class for numeric types that provide certain special functions.
-- class Floating a => NumSpec a where
-- {-# MINIMAL (gamma | logGamma) , (beta | logBeta) #-}
-- -- | Gamma function.
-- gamma :: a -> a
-- gamma = exp . logGamma
-- -- | Beta function.
-- beta  :: a -> a -> a
-- beta a b = exp $ logBeta a b
-- -- | Logarithm of `gamma`.
-- --
-- -- > logGamma = log . gamma
-- logGamma :: a -> a
-- logGamma = log . gamma
-- -- | Logarithm of `beta`
-- --
-- -- > logBeta a b = log (beta a b)
-- logBeta :: a -> a -> a
-- logBeta a b = log $ beta a b
--
-- -- | Efficient sum.
-- sum :: Foldable f => f a -> a
-- sum = Fold.sum
--
-- -- | Divides all elements by their sum.
-- -- Throws an error if the sum of all elements is zero or undefined.
-- -- May return NaN's if the sum is infinite.
-- normalize :: Traversable f => f a -> f a
-- normalize t = Traverse.fmapDefault (/ z) t where
--   z = sum t
--   -- specific instances may provide extra checks to avoid division by 0 etc.
--   -- we can't do that here without restricting the type of a
--
-- instance NumSpec Double where
-- logGamma  = Spec.logGamma
-- logBeta   = Spec.logBeta
--
-- -- just like default, but with extra checks
-- normalize t = Traverse.fmapDefault (/ z) t where
--   z = let z' = sum t in
--         if isInfinite z' then error "Normalize: infinite sum" else
--           if z' > 0 then z' else error "Normalize: bad weights"
--
-- instance (Ord a, NumSpec a) => NumSpec (LogDomain a) where
-- gamma     = liftLog (logGamma . exp)
-- beta a b  = liftLog2 (\x y -> logBeta (exp x) (exp y)) a b
--
-- -- sum is made more efficient by only converting all elements to linear domain once,
-- -- then summing them in linear domain and taking a logarithm of the result
-- -- to prevent overflow all numbers are first divided by their maximum,
-- -- and the final result is mulitplied by it
-- sum t = let z = Fold.maximum t in
--   if z == 0 then
--     -- unlike just returning zero, this ensures that NaNs propagate through
--     toLogDomain (Fold.foldl' (\x y -> x + fromLogDomain y) 0 t)
--   else
--     z * toLogDomain (Fold.foldl' (\x y -> x + fromLogDomain (y / z)) 0 t)
--
-- -- just like default, but with extra checks
-- normalize t = Traverse.fmapDefault (/ z) t where
--   z = let z' = sum t in
--         if z' > 0 then z' else error "Normalize: bad weights"
--
-- -------------------------------------------------
-- -- Automatic Differentiation instances
--
-- instance (Reifies s Tape) => NumSpec (Reverse s Double) where
-- logGamma = lift1 logGamma (Id . Spec.digamma . runId)
-- logBeta  = lift2 logBeta (\(Id x) (Id y) -> (Id (psi x - psi (x+y)), Id (psi y - psi (x+y))))
--   where psi = Spec.digamma
