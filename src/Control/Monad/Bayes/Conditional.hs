{-|
Module      : Control.Monad.Bayes.Conditional
Description : Distributions conditional on some of the variables
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Conditional (
  Conditional,
  conditional,
  pseudoDensity,
  jointDensity,
  contJointDensity,
  unsafeContJointDensity,
  unsafeContJointDensityGradient
) where

import Control.Monad.State
import Data.Maybe
import Control.Arrow (first)

import Data.Reflection
import Numeric.AD
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian
import qualified Numeric.SpecFunctions as Spec

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Deterministic

-- | A probability monad that allows conditioning on any subset of latent variables.
newtype Conditional m a = Conditional (StateT ([Maybe (CustomReal m)], [Maybe Int]) m a)
  deriving (Functor, Applicative, Monad)

type instance CustomReal (Conditional m) = CustomReal m

instance MonadTrans Conditional where
  lift m = Conditional (lift m)

instance MonadBayes m => MonadDist (Conditional m) where
  primitive d = Conditional $ do
    (xs, cs) <- get
    case d of
      Discrete _ | (Just c):cs' <- cs -> do
        factor (pdf d c)
        put (xs, cs')
        return c
      Discrete _ | Nothing:cs' <- cs -> do
        put (xs, cs')
        primitive d
      Continuous _ | (Just x):xs' <- xs -> do
        factor (pdf d x)
        put (xs', cs)
        return x
      Continuous _ | Nothing:xs' <- xs -> do
        put (xs', cs)
        primitive d
      _ -> primitive d

instance MonadBayes m => MonadBayes (Conditional m) where
  factor = lift . factor

-- | Conditional distribution given a subset of random variables.
-- For every fixed value its PDF is included as a `factor`.
-- Discrete and continuous random variables are treated separately.
-- Missing values are treated as no conditioning on that RV.
conditional :: Monad m => Conditional m a -> ([Maybe (CustomReal m)], [Maybe Int]) -> m a
conditional (Conditional m) = evalStateT m

-- | Evaluates joint density of a subset of random variables.
-- Specifically this is a product of conditional densities encountered in
-- the trace times the (unnormalized) likelihood of the trace.
-- Missing latent variables are integrated out using the transformed monad,
-- unused values from the list are ignored.
pseudoDensity :: MonadDist m => Conditional (Weighted m) a -> ([Maybe (CustomReal m)], [Maybe Int]) -> m (LogDomain (CustomReal m))
pseudoDensity m = fmap snd . runWeighted . conditional m

-- | Joint density of all random variables in the program.
-- Failure occurs when the lists are too short.
jointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> ([r], [Int]) -> Maybe (LogDomain r)
jointDensity m (xs,cs) = maybeDeterministic $ pseudoDensity m (map Just xs, map Just cs)

-- | Like 'jointDensity', but assumes all random variables are continuous.
contJointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> [r] -> Maybe (LogDomain r)
contJointDensity m xs = jointDensity m (xs,[])

-- | Like 'contJointDensity', but throws an error if density can not be computed.
unsafeContJointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> [r] -> LogDomain r
unsafeContJointDensity m xs = fromMaybe (error "Could not compute density: some random variables are discrete or the list of random variables is too short") $ contJointDensity m xs


-------------------------------------------------
-- Automatic Differentiation

instance (Reifies s Tape) => NumSpec (Reverse s Double) where
  logGamma = lift1 logGamma (Id . Spec.digamma . runId)
  logBeta  = lift2 logBeta (\(Id x) (Id y) -> (Id (psi x - psi (x+y)), Id (psi y - psi (x+y))))
    where psi = Spec.digamma

-- | Like 'unsafeContJointDensity', but additionally returns the gradient.
-- Note that this is the gradient of the log-likelihood, even though it is not represented using 'LogDomain'.
unsafeContJointDensityGradient :: (forall s. Reifies s Tape =>  Conditional (Weighted (Deterministic (Reverse s Double))) a) -> [Double] -> (LogDomain Double, [Double])
unsafeContJointDensityGradient m xs = first fromLog $ grad' (toLog . unsafeContJointDensity m) xs
