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
import qualified Numeric.LinearAlgebra as LA

import Numeric.LogDomain
import Control.Monad.Bayes.Weighted hiding (hoist)
import Control.Monad.Bayes.Class
import Statistics.Distribution.Polymorphic.MVNormal
import Control.Monad.Bayes.Deterministic
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Simple

-- | A probability monad that allows conditioning on the latent variables.
-- The variables which aren't conditioned on should be lifted from the transformed monad.
newtype Conditional m a = Conditional (StateT ([CustomReal m], [Int]) (MaybeT m) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance HasCustomReal m => HasCustomReal (Conditional m) where
  type CustomReal (Conditional m) = CustomReal m

instance MonadTrans Conditional where
  lift m = Conditional (lift $ lift m)

instance {-# OVERLAPPING #-} (r ~ CustomReal m, Conditionable m, Monad m) => Sampleable (Discrete r Int) (Conditional m) where
  sample d = Conditional $ do
    (xs, cs) <- get
    case cs of
      (c:cs') -> do
        factor (pdf d c)
        put (xs, cs')
        return c
      _ -> fail ""

instance {-# OVERLAPPING #-} (RealNum d ~ CustomReal m, Domain d ~ CustomReal m, Density d, Conditionable m, Monad m) => Sampleable d (Conditional m) where
  sample d = Conditional $ do
    (xs, cs) <- get
    case xs of
      (x:xs') -> do
        factor (pdf d x)
        put (xs', cs)
        return x
      _ -> fail ""

instance {-# OVERLAPPING #-} (CustomReal m ~ Double, Conditionable m, Monad m) => Sampleable MVNormal (Conditional m) where
  sample d = Conditional $ do
    (xs, cs) <- get
    let k = dim d
    let (taken, remaining) = splitAt k xs
    if length taken == k then
      do
        let v = LA.fromList taken
        factor (pdf d v)
        put (remaining, cs)
        return v
    else
      fail ""

instance (Conditionable m, Monad m) => Conditionable (Conditional m) where
  factor = lift . factor

instance MonadBayes m => MonadDist (Conditional m)
instance MonadBayes m => MonadBayes (Conditional m)

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
