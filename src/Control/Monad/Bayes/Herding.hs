{-|
Module      : Control.Monad.Bayes.Herding
Description : Kernel herding algorithms
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Herding (
  module Control.Monad.Bayes.Kernel,
  EmpiricalEmbedding,
  herding
  ) where

import Numeric.LinearAlgebra

import Control.Monad.Bayes.Kernel

-- | Empirical embedding consists of a kernel, a vector of weights,
-- and a list of expansion points.
type EmpiricalEmbedding a = (Kernel R a, Vector R, [a])

-- | Kernel herding algorithm developed by
-- Chen et al. "Super-samples from kernel herding" (2010).
-- Greedily approximates an empirical embedding using a specified number of
-- unweighted pseudo-samples chosen with replacement from the original set.
herding :: EmpiricalEmbedding a -- ^ embedding to be approximated
        -> Int -- ^ number of pseudo-samples used for approximation
        -> [a]
herding m@(_,originalWeights,_) = f m where
  n = size originalWeights
  z = sum $ toList originalWeights
  pointEmbedding i = vector $ replicate i 0 ++ (z : replicate (n-i) 0)
  f _ 0 = []
  f m@(kernel, ws, xs) n = (xs !! i) : f (kernel, ws', xs) (n-1) where
    ws' = ws + originalWeights - pointEmbedding i
    i = bestIndex m

-- | Chooses a point that best approximates the original embedding.
-- Returns its index in the list of points in the embedding.
bestIndex :: EmpiricalEmbedding a -> Int
bestIndex (kernel, ws, xs) = maxIndex scores where
  n = length xs
  k = evalKernel kernel
  kMat = (n><n) [k x y | x <- xs, y <- xs]
  scores = kMat #> ws - scale 0.5 (takeDiag kMat)
