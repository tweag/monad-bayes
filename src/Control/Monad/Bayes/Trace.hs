{-# LANGUAGE
  GADTs,
  GeneralizedNewtypeDeriving,
  DeriveFunctor,
  ScopedTypeVariables,
  StandaloneDeriving,
  Rank2Types,
  TupleSections #-}

module Control.Monad.Bayes.Trace where

import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted

import Control.Arrow
import Control.Monad
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Trans.Class

import Data.List
import Data.Number.LogFloat
import Data.Typeable

-- | An old primitive sample is reusable if both distributions have the
-- same support.
reusePrimitive :: Primitive a -> Primitive b -> a -> Maybe b
reusePrimitive d d' x =
  case (support d, support d') of
    (OpenInterval   a b, OpenInterval   c d) | (a, b) == (c, d)              -> Just x
    (ClosedInterval a b, ClosedInterval c d) | (a, b) == (c, d)              -> Just x
    (Discrete       xs , Discrete       ys ) | maybe False (== ys) (cast xs) -> cast x
    _                                                                        -> Nothing

data Support a where
  OpenInterval   :: Double -> Double -> Support Double
  ClosedInterval :: Double -> Double -> Support Double
  Discrete       :: (Eq a, Typeable a) => [a] -> Support a

support :: Primitive a -> Support a
support (Categorical xs) = Discrete (map fst xs)
support (Normal  _ _)    = OpenInterval (-1/0) (1/0)
support (Gamma   _ _)    = OpenInterval 0 (1/0)
support (Beta    _ _)    = OpenInterval 0 1
support (Uniform a b)    = ClosedInterval a b

data Cache where
  Cache :: Primitive a -> a -> Cache

instance Eq Cache where
  Cache (Categorical ds) x == Cache (Categorical ds') x' = cast x == Just x' && cast ds == Just ds'
  Cache d@(Normal   _ _) x == Cache d'@(Normal   _ _) x' = d == d' && x == x'
  Cache d@(Gamma    _ _) x == Cache d'@(Gamma    _ _) x' = d == d' && x == x'
  Cache d@(Beta     _ _) x == Cache d'@(Beta     _ _) x' = d == d' && x == x'
  Cache d@(Uniform  _ _) x == Cache d'@(Uniform  _ _) x' = d == d' && x == x'

-- Suspension functor: yields primitive distribution, awaits sample.
data AwaitSampler y where
  AwaitSampler :: Primitive a -> (a -> y) -> AwaitSampler y

-- Suspension functor: yield primitive distribution and previous sample, awaits new sample.
data Snapshot y where
  Snapshot :: Primitive a -> a -> (a -> y) -> Snapshot y

deriving instance Functor AwaitSampler
deriving instance Functor Snapshot

snapshotToCache :: Snapshot y -> Cache
snapshotToCache (Snapshot d x _) = Cache d x

-- Pause probabilistic program whenever a primitive distribution is
-- encountered, yield the encountered primitive distribution, and
-- await a sample of that primitive distribution.
newtype Coprimitive m a = Coprimitive
  { runCoprimitive :: Coroutine AwaitSampler m a
  }
  deriving (Functor, Applicative, Monad)

instance (MonadDist m) => MonadDist (Coprimitive m) where
  primitive d = Coprimitive (suspend (AwaitSampler d return))

-- MHState is probably not a monad transformer, since coroutine
-- is not commutative. Consider converting the list of snapshots
-- to ListT-done-right.
data MHState m a = MHState
  { mhSnapshots :: [Snapshot (Weighted (Coprimitive m) a)]
  , mhPosteriorWeight :: LogFloat -- weight of mhAnswer in posterior
  , mhAnswer :: a
  }

-- | Make one MH transition, retain weight from the source distribution
mhTransition :: (MonadDist m) =>
                Weighted m (MHState m a) -> Weighted m (MHState m a)

mhTransition m = withWeight $ do
  (oldState, oldWeight) <- runWeighted m
  newState <- mhKernel oldState
  return (newState, oldWeight)

-- | *The* Metropolis-Hastings kernel. Each MH-state carries all necessary
-- information to compute the acceptance ratio.
mhKernel :: (MonadDist m) => MHState m a -> m (MHState m a)
mhKernel oldState = do
  let m = length (mhSnapshots oldState)
  i <- uniformD [0 .. m - 1]
  let (prev, snapshot : next) = splitAt i (mhSnapshots oldState)
  let caches = map snapshotToCache next
  case snapshot of
    Snapshot d x continuation -> do
      x' <- primitive d
      (reuseRatio, partialState) <- mhReuse caches (continuation x')

      let newSnapshots = prev ++ Snapshot d x' continuation : mhSnapshots partialState
      let newState     = partialState { mhSnapshots = newSnapshots }
      let n            = length newSnapshots
      let numerator    = fromIntegral m * mhPosteriorWeight newState * reuseRatio
      let denominator  = fromIntegral n * mhPosteriorWeight oldState
      let acceptRatio  = if denominator == 0 then 1 else min 1 (numerator / denominator)

      accept <- bernoulli acceptRatio
      return (if accept then newState else oldState)

-- | Convert a model into a distribution of MH-states, each consisting
-- of a list of continuations from each primitive distribution.
mhState :: (MonadDist m) => Weighted (Coprimitive m) a -> m (MHState m a)
mhState m = fmap snd (mhReuse [] m)

-- | Transition a subprobability distribution, preserving its weights
mhWeightedState :: (MonadDist m) => Weighted (Coprimitive m) a -> Weighted m (MHState m a)
mhWeightedState m = withWeight $ do
  state <- mhState m
  return (state, mhPosteriorWeight state)

-- | Forget the state (i. e., a list of continuations), return a weighted final result.
forgetMHState :: (MonadBayes m) => Weighted m (MHState m a) -> m a
forgetMHState m = do
  (state, weight) <- runWeighted m
  factor weight
  return (mhAnswer state)

-- | Reuse previous samples as much as possible, collect reuse ratio
mhReuse :: forall m a. (MonadDist m) =>
                 [Cache] ->
                 Weighted (Coprimitive m) a ->
                 m (LogFloat, MHState m a)

mhReuse caches m = do
  result <- resume (runCoprimitive (runWeighted m))
  case result of

    Right (answer, weight) ->
      return (1, MHState [] weight answer)

    Left (AwaitSampler d' continuation) | (Cache d x : otherCaches) <- caches ->
      let
        weightedCtn = withWeight . Coprimitive . continuation
      in
        case reusePrimitive d d' x of
          Nothing -> do
            x' <- primitive d'
            (reuseRatio, MHState snapshots weight answer) <- mhReuse otherCaches (weightedCtn x')
            return (reuseRatio, MHState (Snapshot d' x' weightedCtn : snapshots) weight answer)
          Just x' -> do
            (reuseRatio, MHState snapshots weight answer) <- mhReuse otherCaches (weightedCtn x')
            let reuseFactor = pdf d' x' / pdf d x
            return (reuseRatio * reuseFactor, MHState (Snapshot d' x' weightedCtn : snapshots) weight answer)

    Left (AwaitSampler d' continuation) | [] <- caches -> do
      x' <- primitive d'
      let weightedCtn = withWeight . Coprimitive . continuation
      (reuseFactor, MHState snapshots weight answer) <- mhReuse [] (weightedCtn x')
      return (reuseFactor, MHState (Snapshot d' x' weightedCtn : snapshots) weight answer)
