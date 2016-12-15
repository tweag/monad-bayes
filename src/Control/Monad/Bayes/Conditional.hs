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
  hoist,
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
import Control.Monad.Bayes.Weighted hiding (hoist)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Deterministic
import Control.Monad.Bayes.Trace hiding (hoist)

-- | A probability monad that allows conditioning on the latent variables.
-- The variables which aren't conditioned on should be lifted from the transformed monad.
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

-- | Applies a transformation to the inner monad.
hoist :: (CustomReal m ~ CustomReal n) => (forall x. m x -> n x) -> Conditional m a -> Conditional n a
hoist f (Conditional m) = Conditional $ mapStateT (mapMaybeT f) m

errorTraceShape :: Maybe a -> a
errorTraceShape = fromMaybe (error "Conditional: Trace shape does not match")

unsafeTraceShape :: Functor m => MaybeT m a -> m a
unsafeTraceShape = fmap errorTraceShape . runMaybeT

-- | Conditional distribution given a subset of random variables.
-- For every fixed value its PDF is included as a `factor` in the transformed monad.
-- 'Nothing' is returned on trace shape mismatch only.
maybeConditional :: Monad m => Conditional m a -> Trace (CustomReal m) -> MaybeT m a
maybeConditional (Conditional m) t = do
  (x, remaining) <- runStateT m (toLists t)
  unless (null (fst remaining) && null (snd remaining)) (fail "")
  return x

-- | Like 'maybeConditional', but throws an error on trace shape mismatch.
unsafeConditional :: Monad m => Conditional m a -> Trace (CustomReal m) -> m a
unsafeConditional m t = unsafeTraceShape $ maybeConditional m t

-- | Computes the joint (pseudo-) density of the random variables in the model.
-- The variables lifted from the transformed monad are not affected.
-- 'Nothing' is returned on trace shape mismatch only.
maybeDensity :: MonadDist m => Conditional (Weighted m) a -> Trace (CustomReal m) -> MaybeT m (LogDomain (CustomReal m))
maybeDensity m t = MaybeT $ do
  (mx, w) <- runWeighted $ runMaybeT $ maybeConditional m t
  return (mx $> w)

-- | Like 'maybeDensity', but throws an error on trace shape mismatch.
unsafeDensity :: MonadDist m => Conditional (Weighted m) a -> Trace (CustomReal m) -> m (LogDomain (CustomReal m))
unsafeDensity m t = unsafeTraceShape $ maybeDensity m t

-- | Computes the joint density of all random variables in the model.
-- 'Nothing' is returned on trace shape mismatch or if not all of the random variables were used for conditioning.
maybeJointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> Trace r -> Maybe (LogDomain r)
maybeJointDensity m t = join $ maybeDeterministic $ runMaybeT $ maybeDensity m t

-- | Like 'maybeJointDensity', but throws an error instead of returning 'Nothing'.
unsafeJointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> Trace r -> LogDomain r
unsafeJointDensity m t = unsafeDeterministic $ unsafeDensity m t

-- | Joint density of all random variables and its gradient.
-- Only continuous random variables are allowed.
-- Throws an error under the same conditions as 'unsafeJointDensity'.
unsafeJointDensityGradient :: (forall s. Reifies s Tape =>  Conditional (Weighted (Deterministic (Reverse s Double))) a) -> [Double] -> (LogDomain Double, [Double])
unsafeJointDensityGradient m xs = first fromLog $ grad' (toLog . unsafeJointDensity m . fromLists . (,[])) xs
