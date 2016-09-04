{-# LANGUAGE
  GADTs,
  GeneralizedNewtypeDeriving,
  DeriveFunctor,
  ScopedTypeVariables,
  StandaloneDeriving,
  Rank2Types,
  TupleSections,
  TypeFamilies,
  FlexibleContexts
   #-}

module Control.Monad.Bayes.Trace where

import Control.Monad.Bayes.LogDomain (LogDomain, toLogDomain, fromLogDomain)
import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted hiding (mapMonad)
import Control.Monad.Bayes.Coprimitive

import Control.Arrow
import Control.Monad
import Control.Monad.Coroutine hiding (mapMonad)
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Trans.Class

import Data.Maybe
import Data.List
import Data.Dynamic
import Safe (tailSafe)

-- | An old primitive sample is reusable if both distributions have the
-- same support.
reusePrimitive :: Primitive r a -> Primitive r b -> a -> Maybe b
reusePrimitive d d' x =
  -- Adam: this could be replaced with first checking if a == b and if so then
  -- if support d == support d'
  case (support d, support d') of
    (Interval   a b, Interval   c d) ->
     do
       (a',b') <- cast (a,b)
       x'      <- cast x
       if (a',b') == (c,d) then Just x' else Nothing
    (Finite         xs , Finite         ys )                    ->
      do
        xs' <- cast xs
        x'  <- cast x
        if xs' == ys then Just x' else Nothing
    _                                                           -> Nothing



data Cache r where
  Cache :: Primitive r a -> a -> Cache r

instance Eq (Cache r) where
  Cache d@(Discrete  _) x == Cache d'@(Discrete  _) x' =
    fromMaybe False (do {p <- cast d; y <- cast x; return (y == x' && p == d')})
  Cache d@(Normal  _ _) x == Cache d'@(Normal  _ _) x' =
    fromMaybe False (do {p <- cast d; y <- cast x; return (p == d' && y == x')})
  Cache d@(Gamma   _ _) x == Cache d'@(Gamma   _ _) x' =
    fromMaybe False (do {p <- cast d; y <- cast x; return (p == d' && y == x')})
  Cache d@(Beta    _ _) x == Cache d'@(Beta    _ _) x' =
    fromMaybe False (do {p <- cast d; y <- cast x; return (p == d' && y == x')})
  Cache d@(Uniform _ _) x == Cache d'@(Uniform _ _) x' =
    fromMaybe False (do {p <- cast d; y <- cast x; return (p == d' && y == x')})
  Cache _               _ == Cache _                _  = False

instance Show r => Show (Cache r) where
  showsPrec p cache r =
    -- Haskell passes precedence 11 for Cache as a constructor argument.
    -- The precedence of function application seems to be 10.
    if p > 10 then
      '(' : printCache cache (')' : r)
    else
      printCache cache r
    where
      printCache :: Cache r -> String -> String
      printCache (Cache d@(Discrete  _) x) r = "Cache " ++ showsPrec 11 d (' ' : showsPrec 11 (show x) r)
      printCache (Cache d@(Normal  _ _) x) r = "Cache " ++ showsPrec 11 d (' ' : showsPrec 11 (show x) r)
      printCache (Cache d@(Gamma   _ _) x) r = "Cache " ++ showsPrec 11 d (' ' : showsPrec 11 (show x) r)
      printCache (Cache d@(Beta    _ _) x) r = "Cache " ++ showsPrec 11 d (' ' : showsPrec 11 (show x) r)
      printCache (Cache d@(Uniform _ _) x) r = "Cache " ++ showsPrec 11 d (' ' : showsPrec 11 (show x) r)



-- Suspension functor: yield primitive distribution and previous sample, awaits new sample.
data Snapshot r y where
  Snapshot :: Typeable a => Primitive r a -> a -> (a -> y) -> Snapshot r y
deriving instance Functor (Snapshot r)

snapshotToCache :: Snapshot r y -> Cache r
snapshotToCache (Snapshot d x _) = Cache d x




data MHState m a = MHState
  { mhSnapshots :: [Snapshot (CustomReal m) (Weighted (Coprimitive m) a)]
    -- continuations in snapshots produce full likelihood of the whole trace
  , mhPosteriorWeight :: LogDomain (CustomReal m) -- likelihood of the trace
  , mhAnswer :: a
  }
  deriving Functor

-- | Reuse previous samples as much as possible, collect reuse ratio
mhReuse :: forall m a. (MonadDist m) =>
                 [Cache (CustomReal m)] ->
                 Weighted (Coprimitive m) a ->
                 m (LogDomain (CustomReal m), MHState m a)
mhReuse caches m = do
  result <- resume (runCoprimitive (runWeighted m))
  case result of
    -- program finished
    Right (answer, weight) ->
      return (1, MHState [] weight answer)
    -- random choice encountered
    Left (AwaitSampler d' continuation) ->
      let
        -- package continuation
        wCtn = withWeight . Coprimitive . continuation
        -- new value and MH correction factor from reuse
        (newSample, reuseFactor) = case caches of
          -- reuse
          (Cache d x : cs) | Just x' <- reusePrimitive d d' x -> (return x', pdf d' x' / pdf d x)
          -- can't reuse - sample from prior
          _ -> (primitive d', 1)
        -- cached value is discarded even on mismatch
        otherCaches = tailSafe caches
      in
        do
          x' <- newSample
          let s = Snapshot d' x' wCtn
          (reuseRatio, MHState snaps w a) <- mhReuse otherCaches (wCtn x')
          return (reuseRatio * reuseFactor, MHState (s : snaps) w a)


-- | Run model once, cache primitive samples together with continuations
-- awaiting the samples
mhState :: (MonadDist m) => Weighted (Coprimitive m) a -> m (MHState m a)
mhState m = fmap snd (mhReuse [] m)

-- | Forget cached samples, return a continuation to execute from scratch
mhReset :: (MonadDist m) => m (MHState m a) -> Weighted (Coprimitive m) a
mhReset m = withWeight $ Coprimitive $ Coroutine $ do
  MHState snapshots weight answer <- m
  case snapshots of
    [] ->
      return $ Right (answer, weight)

    Snapshot d x continuation : _ ->
      return $ Left $ AwaitSampler d (runCoprimitive . runWeighted . continuation)

-- Adam: is it the case that mhReset . mhState = id = mhState . mhReset?

-- | Propose new state, compute acceptance ratio
mhPropose :: (MonadDist m) => MHState m a -> m (MHState m a, LogDomain (CustomReal m))
mhPropose oldState = do
  let m = length (mhSnapshots oldState)
  if m == 0 then
    return (oldState, 1)
  else do
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

        return (newState, acceptRatio)

-- | *The* Metropolis-Hastings kernel. Each MH-state carries all necessary
-- information to compute the acceptance ratio.
mhKernel :: (MonadDist m) => MHState m a -> m (MHState m a)
mhKernel oldState = do
  (newState, acceptRatio) <- mhPropose oldState
  accept <- bernoulli $ fromLogDomain acceptRatio
  return (if accept then newState else oldState)






newtype Trace m a = Trace { unTrace :: LogDomain (CustomReal m) -> m (MHState m a) }
  deriving (Functor)
runTrace :: MonadDist m => Trace m a -> m (MHState m a)
runTrace = (`unTrace` 1)

type instance CustomReal (Trace m) = CustomReal m

instance MonadDist m => Applicative (Trace m) where
  pure  = return
  (<*>) = liftM2 ($)

instance MonadDist m => Monad (Trace m) where

  return x = Trace $ \w -> return (MHState [] w x)

  m >>= f = Trace $ \w -> do
    MHState ls lw la <- unTrace m w
    MHState rs rw ra <- unTrace (f la) lw
    return $ MHState (map (fmap (convert lw)) ls ++ rs) rw ra
    where
      --convert w :: Weighted (Coprimitive m) a -> Weighted (Coprimitive m) b
      convert p = (>>= mhReset . (`unTrace` p) . f)


instance MonadTrans Trace where
  lift m = Trace $ \w -> fmap (MHState [] w) m

instance MonadDist m => MonadDist (Trace m) where
  primitive d = Trace $ \w -> do
    x <- primitive d
    return $ MHState [Snapshot d x (\x -> factor w >> return x)] w x

instance MonadDist m => MonadBayes (Trace m) where
  factor k = Trace $ \w -> return (MHState [] (w * k) ())

mapMonad :: Monad m => (forall x. m x -> m x) -> Trace m x -> Trace m x
mapMonad t (Trace m) = Trace (t . m)

mhStep :: (MonadDist m) => Trace m a -> Trace m a
mhStep (Trace m) = Trace (m >=> mhKernel)

marginal :: MonadDist m => Trace m a -> m a
marginal = fmap mhAnswer . runTrace



-- | Like Trace, except it passes factors to the underlying monad.
newtype Trace' m a = Trace' { runTrace' :: Trace (WeightRecorderT m) a }
  deriving (Functor)
type instance CustomReal (Trace' m) = CustomReal m
deriving instance MonadDist m => Applicative (Trace' m)
deriving instance MonadDist m => Monad (Trace' m)
deriving instance MonadDist m => MonadDist (Trace' m)

type instance CustomReal (Trace' m) = CustomReal m

instance MonadTrans Trace' where
  lift = Trace' . lift . lift

instance MonadBayes m => MonadBayes (Trace' m) where
  factor k = Trace' $ do
    factor k
    lift (factor k)

mapMonad' :: MonadDist m => (forall x. m x -> m x) -> Trace' m x -> Trace' m x
mapMonad' t = Trace' . mapMonad (mapMonadWeightRecorder t) . runTrace'

-- | Make one MH transition, retain weight from the source distribution
mhStep' :: (MonadBayes m) => Trace' m a -> Trace' m a
mhStep' (Trace' m) = Trace' $ Trace $ \w -> lift $ do
  (x,s) <- runWeighted $ duplicateWeight $ unTrace (mhStep $ mapMonad resetWeightRecorder m) w
  -- Reverse the effect of factors in mhStep on the transformed monad.
  factor (1 / s)
  return x

marginal' :: MonadDist m => Trace' m a -> m a
marginal' = fmap fst . runWeighted . duplicateWeight . marginal . runTrace'
