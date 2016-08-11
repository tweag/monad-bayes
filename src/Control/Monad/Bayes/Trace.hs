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
import Control.Monad.Bayes.Deterministic

import Control.Arrow
import Control.Monad
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Trans.Class

import Data.Maybe
import Data.List
import Data.Number.LogFloat
import Data.Dynamic

-- | An old primitive sample is reusable if both distributions have the
-- same support.
reusePrimitive :: Primitive a -> Primitive b -> a -> Maybe b
reusePrimitive d d' x =
  -- Adam: this could be replaced with first checking if a == b and if so then
  -- if support d == support d'
  case (support d, support d') of
    (OpenInterval   a b, OpenInterval   c d) | (a, b) == (c, d) -> Just x
    (ClosedInterval a b, ClosedInterval c d) | (a, b) == (c, d) -> Just x
    (Finite         xs , Finite         ys )                    ->
      do
        xs' <- cast xs
        x'  <- cast x
        if xs' == ys then Just x' else Nothing
    _                                                           -> Nothing

data Support a where
  OpenInterval   :: Double -> Double -> Support Double
  ClosedInterval :: Double -> Double -> Support Double
  Finite         :: (Typeable a, Integral a) => [a] -> Support a

deriving instance Eq   (Support a)
instance Show (Support a) where
  show (Finite xs) = "Finite " ++ show (map toInteger xs)
  show (OpenInterval   a b) = "OpenInterval "  ++ show a ++ " " ++ show b
  show (ClosedInterval a b) = "ClosedInterval" ++ show a ++ " " ++ show b

support :: Primitive a -> Support a
support (Discrete ps) = Finite $ map fromIntegral $ findIndices (> 0) ps
support (Normal  _ _) = OpenInterval (-1/0) (1/0)
support (Gamma   _ _) = OpenInterval 0 (1/0)
support (Beta    _ _) = OpenInterval 0 1
support (Uniform a b) = ClosedInterval a b

data Cache where
  Cache :: Primitive a -> a -> Cache

instance Eq Cache where
  Cache d@(Discrete  _) x == Cache d'@(Discrete  _) x' =
    fromMaybe False (do {p <- cast d; y <- cast x; return (y == x' && p == d')})
  Cache d@(Normal  _ _) x == Cache d'@(Normal  _ _) x' = d == d' && x == x'
  Cache d@(Gamma   _ _) x == Cache d'@(Gamma   _ _) x' = d == d' && x == x'
  Cache d@(Beta    _ _) x == Cache d'@(Beta    _ _) x' = d == d' && x == x'
  Cache d@(Uniform _ _) x == Cache d'@(Uniform _ _) x' = d == d' && x == x'
  Cache _               _ == Cache _                _  = False

instance Show Cache where
  showsPrec p cache r =
    -- Haskell passes precedence 11 for Cache as a constructor argument.
    -- The precedence of function application seems to be 10.
    if p > 10 then
      '(' : printCache cache (')' : r)
    else
      printCache cache r
    where
      printCache :: Cache -> String -> String
      printCache (Cache d@(Discrete  _) x) r = "Cache " ++ showsPrec 11 d (' ' : showsPrec 11 (toInteger x) r)
      printCache (Cache d@(Normal  _ _) x) r = "Cache " ++ showsPrec 11 d (' ' : showsPrec 11 x r)
      printCache (Cache d@(Gamma   _ _) x) r = "Cache " ++ showsPrec 11 d (' ' : showsPrec 11 x r)
      printCache (Cache d@(Beta    _ _) x) r = "Cache " ++ showsPrec 11 d (' ' : showsPrec 11 x r)
      printCache (Cache d@(Uniform _ _) x) r = "Cache " ++ showsPrec 11 d (' ' : showsPrec 11 x r)

-- Suspension functor: yields primitive distribution, awaits sample.
data AwaitSampler y where
  AwaitSampler :: Typeable a => Primitive a -> (a -> y) -> AwaitSampler y

-- Suspension functor: yield primitive distribution and previous sample, awaits new sample.
data Snapshot y where
  Snapshot :: Typeable a => Primitive a -> a -> (a -> y) -> Snapshot y

deriving instance Functor AwaitSampler
deriving instance Functor Snapshot

snapshotToCache :: Snapshot y -> Cache
snapshotToCache (Snapshot d x _) = Cache d x

-- | Pause probabilistic program whenever a primitive distribution is
-- encountered, yield the encountered primitive distribution, and
-- await a sample of that primitive distribution.
newtype Coprimitive m a = Coprimitive
  { runCoprimitive :: Coroutine AwaitSampler m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans)

instance (MonadDist m) => MonadDist (Coprimitive m) where
  primitive d = Coprimitive (suspend (AwaitSampler d return))

instance (MonadBayes m) => MonadBayes (Coprimitive m) where
  factor = lift . factor

-- | Conditional distribution given a subset of random variables.
-- For every fixed value its density is included as a factor.
-- Missing values and type mismatches are treated as no conditioning on that RV.
conditional :: MonadBayes m => Coprimitive m a -> [Maybe Dynamic] -> m a
conditional p xs = condRun (runCoprimitive p) xs where
  -- Draw a value from the prior and proceed.
  drawFromPrior :: MonadDist m => AwaitSampler a -> m a
  drawFromPrior (AwaitSampler d k) = fmap k (primitive d)

  -- No more variables to condition on - run from prior.
  condRun c [] = pogoStickM drawFromPrior c
  -- Condition on the next variable.
  condRun c (x:xs) = resume c >>= \t -> case t of
    -- Program finished - return the final result
    Right y -> return y
    -- Random draw encountered - conditional execution
    Left s@(AwaitSampler d k) ->
      case (x >>= fromDynamic) of
        -- A value is available and its type matches the current draw
        Just v  -> factor (pdf d v) >> condRun (k v) xs
        -- Value missing or type mismatch - ignore and draw from prior
        Nothing -> drawFromPrior s >>= (`condRun` xs)

-- | Evaluates joint density of a subset of random variables.
-- Specifically this is a product of conditional densities encountered in
-- the trace times the (unnormalized) likelihood of the trace.
-- Missing latent variables are integrated out using the transformed monad,
-- unused values from the list are ignored.
pseudoDensity :: MonadBayes m => Coprimitive (Weighted m) a -> [Maybe Dynamic]
  -> m LogFloat
pseudoDensity p xs = fmap snd $ runWeighted $ conditional p xs

-- | Joint density of all random variables in the program.
-- Failure occurs when the list is too short or when there's a type mismatch.
jointDensity :: Coprimitive (Weighted Deterministic) a -> [Dynamic]
  -> Maybe LogFloat
jointDensity p xs = maybeDeterministic $ pseudoDensity p (map Just xs)



data MHState m a = MHState
  { mhSnapshots :: [Snapshot (Weighted (Coprimitive m) a)]
  , mhPosteriorWeight :: LogFloat -- weight of mhAnswer in posterior
  , mhAnswer :: a
  }
  deriving Functor

-- | Run model once, cache primitive samples together with continuations
-- awaiting the samples
mhState :: (MonadDist m) => Weighted (Coprimitive m) a -> m (MHState m a)
mhState m = fmap snd (mhReuse [] m)

-- | Forget cached samples, return a continuation to execute from scratch
mhReset :: (Monad m) => m (MHState m a) -> Weighted (Coprimitive m) a
mhReset m = withWeight $ Coprimitive $ Coroutine $ do
  MHState snapshots weight answer <- m
  case snapshots of
    [] ->
      return $ Right (answer, weight)

    Snapshot d x continuation : _ ->
      return $ Left $ AwaitSampler d (runCoprimitive . runWeighted . continuation)

newtype Trace m a = Trace { runTrace :: m (MHState m a) }
  deriving (Functor)

instance Monad m => Applicative (Trace m) where
  pure  = return
  (<*>) = liftM2 ($)

instance Monad m => Monad (Trace m) where

  return = Trace . return . MHState [] 1

  m >>= f = Trace $ do
    MHState ls lw la <- runTrace m
    MHState rs rw ra <- runTrace (f la)
    return $ MHState (map (fmap convert) ls ++ map (fmap (addFactor lw)) rs) (lw * rw) ra
    where
      --convert :: Weighted (Coprimitive m) a -> Weighted (Coprimitive m) b
      convert = (>>= mhReset . runTrace . f)

      -- addFactor :: LogFloat -> Weighted (Coprimitive m) b -> Weighted (Coprimitive m) b
      addFactor k = (withWeight (return ((), k)) >>)

instance MonadTrans Trace where
  lift = Trace . fmap (MHState [] 1)

instance MonadDist m => MonadDist (Trace m) where
  primitive d = Trace $ do
    x <- primitive d
    return $ MHState [Snapshot d x return] 1 x

instance MonadDist m => MonadBayes (Trace m) where
  factor k = Trace $ return $ MHState [] k ()

-- | Like Trace, except it passes factors to the underlying monad.
newtype Trace' m a = Trace' { runTrace' :: Trace (WeightRecorderT m) a }
  deriving (Functor, Applicative, Monad, MonadDist)

instance MonadTrans Trace' where
  -- lift :: m a -> Trace' m a
  lift = Trace' . lift . lift

instance MonadBayes m => MonadBayes (Trace' m) where
  factor k = Trace' $ do
    factor k
    lift (factor k)

mapMonad :: Monad m => (forall x. m x -> m x) -> Trace m x -> Trace m x
mapMonad nat (Trace m) = Trace (nat m)

mapMonad' :: Monad m => (forall x. m x -> m x) -> Trace' m x -> Trace' m x
mapMonad' nat (Trace' (Trace (WeightRecorderT w))) = Trace' (Trace (WeightRecorderT (withWeight (nat (runWeighted w)))))

mhStep :: (MonadDist m) => Trace m a -> Trace m a
mhStep (Trace m) = Trace (m >>= mhKernel)

-- | Make one MH transition, retain weight from the source distribution
mhStep' :: (MonadBayes m) => Trace' m a -> Trace' m a
mhStep' (Trace' (Trace (WeightRecorderT w))) = Trace' $ Trace $ WeightRecorderT $ withWeight $ do
  (oldState, oldWeight) <- runWeighted w
  ((newState, acceptRatio), newWeight) <- runWeighted $ duplicateWeight $ mhPropose oldState

  accept <- bernoulli acceptRatio
  let nextState = if accept then newState else oldState

  -- at this point, the score in the underlying monad `m` is proportional to `oldWeight * newWeight`.
  factor (1 / newWeight)
  -- at this point, the score in the underlying monad `m` is proportional to  `oldWeight`.

  return (nextState, oldWeight)

mhForgetWeight :: Monad m => Trace' m a -> Trace' m a
mhForgetWeight (Trace' (Trace m)) = Trace' $ Trace $ do
  state <- m
  return $ state {mhPosteriorWeight = 1}

marginal :: Functor m => Trace m a -> m a
marginal = fmap mhAnswer . runTrace

marginal' :: Functor m => Trace' m a -> m a
marginal' = fmap fst . runWeighted . runWeightRecorderT . marginal . runTrace'

-- | Propose new state, compute acceptance ratio
mhPropose :: (MonadDist m) => MHState m a -> m (MHState m a, LogFloat)
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
  accept <- bernoulli acceptRatio
  return (if accept then newState else oldState)


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
