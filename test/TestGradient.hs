{-# LANGUAGE
  FlexibleInstances,
  FlexibleContexts,
  TypeOperators
 #-}

module TestGradient where

import Data.AEq
import Control.Arrow (first)

import qualified Control.Monad.Bayes.LogDomain as Log
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Deterministic
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Conditional

model :: MonadBayes m => m ()
model = do
  x <- gamma 1 1
  y <- normal 0 x
  z <- beta 1 1
  factor $ Log.toLogDomain (x + y * y)
  return ()

(logDensity, gradLogDensity) = first Log.toLog $ unsafeContJointDensityGradient model [1,1,0.5::Double]

check_density = logDensity ~== -1 -0.5*log(2*pi) -0.5 + log 2
check_gradient = gradLogDensity ~== [-0.5, 0, 0]
