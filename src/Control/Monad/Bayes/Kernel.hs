{-|
Module      : Control.Monad.Bayes.Kernel
Description : Kernels for use with kernel methods
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Kernel (
  Kernel,
  evalKernel,
  compose,
  unsafeKernel,
  constant,
  gaussian,
  kernelMatrix,
  rkhsProduct,
  mmd
  ) where

import Numeric.LinearAlgebra
import Data.Bifunctor (second)

-- | A positive semidefinite real-valued symmetric function of two arguments.
-- The first parameter is the type used to represent real numbers,
-- the second parameter is input domain.
newtype Kernel r a = Kernel (a -> a -> r)

-- | Evaluate the kernel at specific points.
evalKernel :: Kernel r a -> a -> a -> r
evalKernel (Kernel k) = k

-- | Compose the kernel with an arbitrary pre-processing function.
-- The result is always a valid kernel regerdless of the function used.
compose :: Kernel r a -> (b -> a) -> Kernel r b
compose k f = unsafeKernel $ \x y -> evalKernel k (f x) (f y)

-- | Construct a kernel from an arbitrary function.
-- No checks are performed to ensure that the function has the required properties.
unsafeKernel :: (a -> a -> r) -> Kernel r a
unsafeKernel = Kernel

-- | A constant kernel that ignores inputs.
constant :: (Ord r, Real r) => r -> Kernel r a
constant c | c >= 0 = unsafeKernel $ const $ const c
constant c = error $ "Attempted to construct a constant kernel of value " ++ show (toRational c)

-- | A Gaussian RBF kernel with specified width.
gaussian :: (Real r, Floating r) => r -> Kernel r r
gaussian sigma | sigma >= 0 = unsafeKernel $ \x y ->
                                exp (- sq (x - y) / (2 * sq sigma)) where
                                  sq = (^ (2 :: Int))
gaussian sigma = error $ "Attempted to construct gaussian kernel with width " ++ show (toRational sigma)

-- | Constructs a matrix of kernel function applied to all pairs of points
-- where the first dimension is over the first list
-- and the second over the second.
kernelMatrix :: Kernel R a -> [a] -> [a] -> Matrix R
kernelMatrix k xs ys =
  (length xs >< length ys) [evalKernel k x y | x <- xs, y <- ys]

-- | Inner product in the RKHS of the two empirical embeddings.
rkhsProduct :: Kernel R a -> [(a,R)] -> [(a,R)] -> R
rkhsProduct k p q = (v <# kernelMatrix k xs ys) <.> u where
  (xs, v) = second vector $ unzip p
  (ys, u) = second vector $ unzip q

-- | Maximum mean discrepancy of the two weighted samples.
mmd :: Kernel R a -> [(a,R)] -> [(a,R)] -> R
mmd k p q = rkhsProduct k p p + rkhsProduct k q q - 2 * rkhsProduct k p q
