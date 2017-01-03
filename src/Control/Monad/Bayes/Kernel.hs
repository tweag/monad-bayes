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
  gaussian
  ) where

-- | A positive semidefinite real-valued symmetric function of two arguments.
-- The first parameter is the type used to represent real numbers,
-- the second parameter is input domain.
newtype Kernel r a = Kernel (a -> a -> r)

-- Evaluate the kernel at specific points.
evalKernel :: Kernel r a -> a -> a -> r
evalKernel (Kernel k) = k

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
                                exp (- (x - y)^2 / (2 * sigma^2))
gaussian sigma = error $ "Attempted to construct gaussian kernel with width " ++ show (toRational sigma)
