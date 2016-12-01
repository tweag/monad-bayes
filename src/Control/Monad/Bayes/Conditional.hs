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
  maybeConditional,
  unsafeConditional,
  maybeDensity,
  unsafeDensity,
  maybeJointDensity,
  unsafeJointDensity,
  unsafeJointDensityGradient
) where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Functor
import Control.Arrow (first)
import Data.Reflection
import Numeric.AD
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Deterministic
import Control.Monad.Bayes.Augmented

-- | A probability monad that allows conditioning on the latent variables.
newtype Conditional m a = Conditional (StateT ([CustomReal m], [Int]) (MaybeT m) a)
  deriving (Functor, Applicative, Monad)

type instance CustomReal (Conditional m) = CustomReal m

instance MonadTrans Conditional where
  lift m = Conditional (lift $ lift m)

instance MonadBayes m => MonadDist (Conditional m) where
  primitive d = Conditional $ do
    (xs, cs) <- get
    case d of
      Discrete _ | c:cs' <- cs -> do
        factor (pdf d c)
        put (xs, cs')
        return c
      Continuous _ | x:xs' <- xs -> do
        factor (pdf d x)
        put (xs', cs)
        return x
      _ -> fail ""

instance MonadBayes m => MonadBayes (Conditional m) where
  factor = lift . factor

-- | Conditional distribution given a subset of random variables.
-- For every fixed value its PDF is included as a `factor`.
-- Discrete and continuous random variables are treated separately.
-- Missing values are treated as no conditioning on that RV.
maybeConditional :: Monad m => Conditional m a -> Trace (CustomReal m) -> MaybeT m a
maybeConditional (Conditional m) t = do
  (x, remaining) <- runStateT m (fromTrace t)
  unless (null (fst remaining) && null (snd remaining)) (fail "")
  return x

errorTraceShape :: Maybe a -> a
errorTraceShape = fromMaybe (error "Conditional: Trace shape does not match")

unsafeTraceShape :: Functor m => MaybeT m a -> m a
unsafeTraceShape = fmap errorTraceShape . runMaybeT

unsafeConditional :: Monad m => Conditional m a -> Trace (CustomReal m) -> m a
unsafeConditional m t = unsafeTraceShape $ maybeConditional m t

maybeDensity :: MonadDist m => Conditional (Weighted m) a -> Trace (CustomReal m) -> MaybeT m (LogDomain (CustomReal m))
maybeDensity m t = MaybeT $ do
  (mx, w) <- runWeighted $ runMaybeT $ maybeConditional m t
  return (mx $> w)

unsafeDensity :: MonadDist m => Conditional (Weighted m) a -> Trace (CustomReal m) -> m (LogDomain (CustomReal m))
unsafeDensity m t = unsafeTraceShape $ maybeDensity m t

maybeJointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> Trace r -> Maybe (LogDomain r)
maybeJointDensity m t = join $ maybeDeterministic $ runMaybeT $ maybeDensity m t

unsafeJointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> Trace r -> LogDomain r
unsafeJointDensity m t = unsafeDeterministic $ unsafeDensity m t

unsafeJointDensityGradient :: (forall s. Reifies s Tape =>  Conditional (Weighted (Deterministic (Reverse s Double))) a) -> [Double] -> (LogDomain Double, [Double])
unsafeJointDensityGradient m xs = first fromLog $ grad' (toLog . unsafeJointDensity m . toTrace . (,[])) xs


--
--
-- -- | Evaluates joint density of a subset of random variables.
-- -- Specifically this is a product of conditional densities encountered in
-- -- the trace times the (unnormalized) likelihood of the trace.
-- -- Missing latent variables are integrated out using the transformed monad,
-- -- unused values from the list are ignored.
-- pseudoDensity :: MonadDist m => Conditional (Weighted m) a -> ([Maybe (CustomReal m)], [Maybe Int]) -> m (LogDomain (CustomReal m))
-- pseudoDensity m = fmap snd . runWeighted . conditional m
--
-- -- | Joint density of all random variables in the program.
-- -- Failure occurs when the lists are too short.
-- jointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> ([r], [Int]) -> Maybe (LogDomain r)
-- jointDensity m (xs,cs) = maybeDeterministic $ pseudoDensity m (map Just xs, map Just cs)
--
-- -- | Like 'jointDensity', but assumes all random variables are continuous.
-- contJointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> [r] -> Maybe (LogDomain r)
-- contJointDensity m xs = jointDensity m (xs,[])
--
-- -- | Like 'contJointDensity', but throws an error if density can not be computed.
-- unsafeContJointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> [r] -> LogDomain r
-- unsafeContJointDensity m xs = fromMaybe (error "Could not compute density: some random variables are discrete or the list of random variables is too short") $ contJointDensity m xs
--
--
-- -------------------------------------------------
-- -- Automatic Differentiation
--
-- -- | Like 'unsafeContJointDensity', but additionally returns the gradient.
-- -- Note that this is the gradient of the log-likelihood, even though it is not represented using 'LogDomain'.
-- unsafeContJointDensityGradient :: (forall s. Reifies s Tape =>  Conditional (Weighted (Deterministic (Reverse s Double))) a) -> [Double] -> (LogDomain Double, [Double])
-- unsafeContJointDensityGradient m xs = first fromLog $ grad' (toLog . unsafeContJointDensity m) xs
