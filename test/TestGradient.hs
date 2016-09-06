{-# LANGUAGE
  FlexibleInstances,
  FlexibleContexts,
  TypeOperators
 #-}

module TestGradient where

import Data.Reflection
import Numeric.AD
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian
import qualified Numeric.SpecFunctions as Spec
import Data.AEq

import qualified Control.Monad.Bayes.LogDomain as Log
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Deterministic
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Coprimitive

instance (Reifies s Tape) => Log.NumSpec (Reverse s Double) where
  logGamma = lift1 Log.logGamma (Id . Spec.digamma . runId)
  logBeta  = lift2 Log.logBeta (\(Id x) (Id y) -> (Id (psi x - psi (x+y)), Id (psi y - psi (x+y))))
    where psi = Spec.digamma

model :: MonadBayes m => m ()
model = do
  x <- gamma 1 1
  y <- normal 0 x
  z <- beta 1 1
  factor $ Log.toLogDomain (x + y * y)
  return ()

(logDensity, gradLogDensity) = grad' (Log.toLog . unsafeContJointDensity model) [1,1,0.5::Double]

check_density = logDensity ~== -1 -0.5*log(2*pi) -0.5 + log 2
check_gradient = gradLogDensity ~== [-0.5, 0, 0]
