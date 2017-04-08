{-|
Module      : Numeric.Optimization.SGD
Description : Stochastic gradient descent
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Numeric.Optimization.SGD (
  SGDParam,
  sgd
) where

data SGDParam = SGDParam

sgd :: (Monad m, Traversable t, Num r) => SGDParam -> (t r -> m (t r)) -> t r -> m (t r)
sgd = undefined
