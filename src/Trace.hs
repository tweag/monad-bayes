{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances,
  GADTs,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  Rank2Types,
  ScopedTypeVariables,
  StandaloneDeriving,
  UndecidableInstances
 #-}


module Trace where

import Control.Arrow
import Control.Monad (liftM, liftM2, mplus)
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Typeable
import Data.Number.LogFloat hiding (sum)
import System.Random (mkStdGen)
import Text.Printf

import Debug.Trace

import Primitive
import Base
import Dist (normalize)
import Sampler



data Cache where
  Cache :: Primitive a -> a -> Cache

refresh :: (MonadDist m) => Cache -> m Cache
refresh (Cache d x) = do
  x' <- primitive d
  return $ Cache d x'

class Monoid r => RandomDB r where
  {-# MINIMAL (singleton | record), consult, mutate, size, weight, resampled #-}
  -- constructors
  record    :: Primitive a -> a -> r
  record d x = singleton (Cache d x)
  singleton :: Cache -> r
  singleton (Cache d x) = record d x

  -- mutators
  consult   :: Primitive a -> r -> Maybe (Cache, r)
  mutate    :: (MonadDist m) => r -> m r

  size :: r -> Int
  weight :: r -> LogFloat
  resampled :: r -> r -> r

  -- proposalFactor old new = q(old, new) / (weight $ resampled old new)
  --
  -- By default, assume that the proposal resamples a stochastic choice
  -- in the old trace uniformly at random. In this case, the proposal factor is
  -- the reciprocal of the number of stochastic choices in the old trace.
  proposalFactor :: r -> r -> Maybe LogFloat
  proposalFactor old new = let denom = size old in if denom /= 0 then Just (1 / fromIntegral denom) else Nothing

-- Correct usage:
-- `old` and `new` traces should both be feasible and
-- the probability of proposing from each other should be
-- positive.
--
-- If `fromJust` throws "Non-exhaustive pattern" error,
-- then the caller did not conform to correct usage.
mhCorrectionFactor :: (RandomDB r) => r -> r -> LogFloat
mhCorrectionFactor old new = fromJust $ do
    pf_new_old <- proposalFactor new old
    pf_old_new <- proposalFactor old new
    let numerator   = weight new * weight (resampled new old) * pf_new_old
    let denominator = weight old * weight (resampled old new) * pf_old_new
    if denominator == 0 then
      Nothing
    else
      Just $ numerator / denominator
      -- should not take mininimum with 1 here

-- | An old primitive sample is reusable if both distributions have the
-- same type and if old sample does not decrease acceptance ratio by
-- more than a threshold.
reusablePrimitive :: Primitive a -> a -> Primitive b -> Maybe ((b, LogFloat), b -> Bool)
reusablePrimitive d x d' =
  let
    threshold = 0.0
  in
    reusablePrimitiveDouble threshold d x d' `mplus` reusableCategorical threshold d x d'

-- | Try to reuse a sample from a categorical distribution
-- if it does not decrease acceptance ration by more than a threshold.
reusableCategorical :: LogFloat -> Primitive a -> a -> Primitive b -> Maybe ((b, LogFloat), b -> Bool)
reusableCategorical threshold d@(Categorical _) x d'@(Categorical _) = do
  x' <- cast x
  let pOld = pdf d x
  let pNew = pdf d' x'
  if pOld > 0 && pNew / pOld > threshold then
    Just ((x', pNew), (== x'))
  else
    Nothing
reusableCategorical threshold _ _ _ = Nothing

-- | An old primitive sample of type Double is reused if old sample does
-- not decrease acceptance ratio by more than a threshold.
-- In particular, a sample is always reused if its distribution did not
-- change, since it does not decrease acceptance ratio at all.
reusablePrimitiveDouble :: LogFloat -> Primitive a -> a -> Primitive b -> Maybe ((b, LogFloat), b -> Bool)
reusablePrimitiveDouble threshold d x d' =
  case (isPrimitiveDouble d, isPrimitiveDouble d') of
    (Just (from, to), Just (from', to')) ->
      let
        x' = from' $ to x
        pOld = pdf d x
        pNew = pdf d' x'
      in
        if pOld > 0 && pNew / pOld > threshold then
          Just ((x', pNew), (== to' x') . to')
        else
          Nothing
    otherwise ->
      Nothing

-- | Test whether a primitive distribution has type @Double@,
-- output conversions to and from if it does.
isPrimitiveDouble :: Primitive a -> Maybe (Double -> a, a -> Double)
isPrimitiveDouble (Normal _ _) = Just (id, id)
isPrimitiveDouble (Gamma  _ _) = Just (id, id)
isPrimitiveDouble (Beta   _ _) = Just (id, id)
isPrimitiveDouble _            = Nothing

-- | Return whether a sample @x@ drawn from @d@ is reused
-- as a sample $x'$ drawn from $d'$.
reusedSample :: Primitive a -> a -> Primitive b -> b -> Bool
reusedSample d x d' x' =
  case reusablePrimitive d x d' of
    Just (_, isReused) -> isReused x'
    Nothing      -> False

------------------------------
-- TRACET MONAD TRANSFORMER --
------------------------------

newtype TraceT r m a = TraceT (WriterT r m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

deriving instance (Monoid r, Monad m) => MonadWriter r (TraceT r m)

runTraceT :: TraceT r m a -> m (a, r)
runTraceT (TraceT writer) = runWriterT writer

mapMonad :: (m (a,r) -> n (a,r)) -> TraceT r m a -> TraceT r n a
mapMonad t (TraceT d) = TraceT $ mapWriterT t d

-- WriterT preserves MonadDist
instance (RandomDB r, MonadDist m) => MonadDist (TraceT r m) where
  primitive d = do
    x <- lift $ primitive d
    tell $ record d x
    return x

-- WriterT preserves MonadBayes
instance (RandomDB r, MonadBayes m) => MonadBayes (TraceT r m) where
  factor = lift . factor
  condition = lift . condition

------------------------------
-- REUSET MONAD TRANDFORMER --
------------------------------

newtype ReuseT r m a = ReuseT (StateT r m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

deriving instance (Monad m) => MonadState r (ReuseT r m)

runReuseT :: ReuseT r m a -> r -> m (a, r)
runReuseT (ReuseT state) = runStateT state

-- Reuse samples of primitive distributions in the RandomDB
instance (RandomDB r, MonadDist m) => MonadDist (ReuseT r m) where
  primitive d' = do
    r <- get
    let resample = lift $ primitive d'
    case consult d' r of
      Nothing ->
        resample
      Just (Cache d x, r') -> do
        put r'
        maybe resample return (fmap (fst . fst) $ reusablePrimitive d x d')

instance (RandomDB r, MonadBayes m) => MonadBayes (ReuseT r m) where
  factor = lift . factor
  condition = lift . condition

type UpdaterT r m a = TraceT r (ReuseT r m) a
runUpdaterT = runReuseT . runTraceT
