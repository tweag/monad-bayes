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


module Control.Monad.Bayes.Trace where

import Control.Arrow
import Control.Monad (liftM, liftM2, mplus)
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Maybe (isJust, fromJust, fromMaybe, maybe)
import Data.Typeable
import Data.Number.LogFloat hiding (sum)
import System.Random (mkStdGen)
import Text.Printf

import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Dist (normalize)
import Control.Monad.Bayes.Sampler



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
-- same support.
reusablePrimitive :: Primitive a -> a -> Primitive b -> Maybe ((b, LogFloat), b -> Bool)
reusablePrimitive d x d' =
  do
    (cast, eq) <- compareSupport (getSupport d) (getSupport d')
    let x' = cast x
    return ((x', pdf d' x'), eq x')

data Support a where
  Real           :: Support Double
  PositiveReal   :: Support Double
  OpenInterval   :: Double -> Double -> Support Double
  ClosedInterval :: Double -> Double -> Support Double
  Discrete       :: (Eq a, Typeable a) => [a] -> Support a

getSupport :: Primitive a -> Support a
getSupport (Categorical xs) = Discrete (map fst xs)
getSupport (Normal  _ _)    = Real
getSupport (Gamma   _ _)    = PositiveReal
getSupport (Beta    _ _)    = OpenInterval 0 1
getSupport (Uniform a b)    = ClosedInterval a b

compareSupport :: Support a -> Support b -> Maybe ((a -> b), (b -> b -> Bool))
compareSupport  Real                 Real                 = Just (id, (==))
compareSupport  PositiveReal         PositiveReal         = Just (id, (==))
compareSupport  (OpenInterval   a b) (OpenInterval   c d) = if [a, b] == [c, d] then Just (id, (==)) else Nothing
compareSupport  (ClosedInterval a b) (ClosedInterval c d) = if [a, b] == [c, d] then Just (id, (==)) else Nothing
compareSupport  (Discrete       xs)  (Discrete       ys)  = if maybe False (== ys) (cast xs) then Just (fromJust . cast, (==)) else Nothing
compareSupport  _                   _                     = Nothing

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
