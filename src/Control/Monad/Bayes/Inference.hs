{-# LANGUAGE
  FlexibleContexts,
  ScopedTypeVariables,
  Rank2Types,
  TupleSections,
  GeneralizedNewtypeDeriving
 #-}

module Control.Monad.Bayes.Inference where

import Control.Arrow (first,second)
import Data.Either
import Data.Number.LogFloat
import Data.Typeable
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Control.Monad.Identity (Identity (Identity), runIdentity)
import Control.Monad.Trans.Identity (IdentityT (IdentityT), runIdentityT)
import Control.Monad.Coroutine (Coroutine (Coroutine), resume)
import Control.Monad.Coroutine.SuspensionFunctors (Await)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Rejection
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Particle as Particle
import Control.Monad.Bayes.Empirical
import Control.Monad.Bayes.Dist
import Control.Monad.Bayes.Prior
import Control.Monad.Bayes.Trace

import Data.List (partition)

-- | Rejection sampling.
rejection :: MonadDist m => Rejection m a -> m a
rejection d = do
  m <- runRejection d
  case m of Just x  -> return x
            Nothing -> rejection d

-- | Simple importance sampling from the prior.
importance :: MonadDist m => Weighted m a -> m (a,LogFloat)
importance = runWeighted

-- | Multiple importance samples with post-processing.
importance' :: (Ord a, Typeable a, MonadDist m) =>
               Int -> Empirical m a -> m [(a,Double)]
importance' n d = fmap (enumerate . categorical) $ runEmpirical $ spawn n >> d

-- | Sequential Monte Carlo from the prior.
-- The first argument is the number of resampling points, the second is
-- the number of particles used.
-- If the first argument is smaller than the number of observations in the model,
-- the algorithm is still correct, but doesn't perform resampling after kth time.
smc :: MonadDist m => Int -> Int -> Particle (Empirical m) a -> Empirical m a
smc k n = smcWithResampler (resampleN n) k n

-- | `smc` with post-processing.
smc' :: (Ord a, Typeable a, MonadDist m) => Int -> Int ->
        Particle (Empirical m) a -> m [(a,Double)]
smc' k n d = fmap (enumerate . categorical) $ runEmpirical $ smc k n d

-- | Common code of 'smcFast'' and 'resampleMoveSMC'
smcSkeleton :: forall t m a s.
               (MonadTrans t, MonadBayes (t (Empirical m)), MonadDist m, Functor s) =>
               Int                                                -> -- number of particles
               (forall x. t (Empirical m) x -> Empirical m (s x)) -> -- lower
               (forall y. s y -> Empirical m (s y))               -> -- kernel
               (forall z. s z -> z)                               -> -- answer
               Particle (t (Empirical m)) a -> Empirical m a
smcSkeleton n lower kernel answer d =
  fmap answer (fromList (start >>= loop)) where
  -- add 1 suspension point at the end so that no particle can terminate
  -- without running into any suspension point
  finalFactor x = factor 1 >> return x

  -- spawn n particles at the start, add suspension point at the end
  start = runLowerResume (lift (lift $ spawn n) >> d >>= finalFactor)

  runLowerResume = runEmpirical . lower . resume

  loop :: [(s (Either (Await () (Particle (t (Empirical m)) a)) a), LogFloat)] -> m [(s a, LogFloat)]
  loop weightedStates =
    if null weightedStates then
      return []
    else do
      -- resample step
      resampled <- resampleList weightedStates

      -- partition particles based on whether they terminated or not
      let (notDoneEW, doneEW) = partition (isLeft . answer . fst) resampled
      let notDone = map (first (fmap fromLeft )) notDoneEW :: [(s(Particle(t(Empirical m))a), LogFloat)]
      let done    = map (first (fmap fromRight)) doneEW    :: [(s a, LogFloat)]
      if null notDone then
        return done
      else do
        -- move step
        let (notDoneStates, notDoneWeights) = unzip notDone
        movedStatesWithWeight <- fmap concat $ sequence $ map (runEmpirical . kernel) notDoneStates
        let (movedStates, ignoredWeights) = unzip movedStatesWithWeight

        -- run particles to the next suspension point
        let nextParticles = map answer movedStates :: [Particle (t (Empirical m)) a]
        nextWeightedStates <- fmap concat $ sequence $ map runLowerResume nextParticles

        -- adjust weight before, then descend with all particles
        let nextAdjustedStates = zipWith (second . (*)) notDoneWeights nextWeightedStates
        let doneStates = map (first (fmap Right)) done
        loop (doneStates ++ nextAdjustedStates)

  fromRight (Right x) = x
  fromRight (Left  x) = error "Should not happen (coroutine did not terminate)"

  fromLeft  (Right x) = return  x
  fromLeft  (Left  x) = extract x

-- | 'smcFast' implemented via 'smcSkeleton'
smcFast' :: forall m a. MonadDist m => Int -> Particle (Empirical m) a -> Empirical m a
smcFast' n d =
  smcSkeleton n (fmap Identity . runIdentityT) -- lower
                return                         -- Dirac kernel
                runIdentity                    -- answer
                (insertIdentityT d)
  where
    -- working around Haskell type parameters not being able to instantiate into
    -- arbitrary type functions (in this case, the identity type function)
    insertIdentityT :: forall x. Particle (Empirical m) x -> Particle (IdentityT (Empirical m)) x
    insertIdentityT = Coroutine . IdentityT . fmap (either (Left . fmap insertIdentityT) Right) . resume

-- Private type of 'resampleMoveSMC'
-- To work around Haskell type parameters not being able to instantiate into
-- arbitrary type functions (in this case, @WeightRecorderT . Coprimitive@)
newtype RM m a = RM { runRM :: (WeightRecorderT (Coprimitive m) a) }
  deriving (Applicative, Functor, Monad, MonadDist, MonadBayes)

instance MonadTrans RM where
  lift = RM . lift . lift

-- | SMC where particles undergo 1 step of Markov transition after resampling
resampleMoveSMC :: forall m a. MonadDist m => Int ->
                   Particle (WeightRecorderT (Coprimitive (Empirical m))) a ->
                   Empirical m a
resampleMoveSMC n =
  smcSkeleton n lower kernel mhAnswer . insertRM
  where
    insertRM :: Particle (WeightRecorderT (Coprimitive (Empirical m))) a ->
                Particle (RM (Empirical m)) a
    insertRM = Coroutine . RM . fmap (either (Left . fmap insertRM) Right) . resume

    lower :: forall x. RM (Empirical m) x -> Empirical m (MHState (Empirical m) x)
    lower = mhState . duplicateWeight . runRM

    kernel :: forall y. MHState (Empirical m) y -> Empirical m (MHState (Empirical m) y)
    kernel = mhKernel

-- | Asymptotically faster version of 'smc' that resamples using multinomial
-- instead of a sequence of categoricals.
smcFast :: MonadDist m => Int -> Int -> Particle (Empirical m) a -> Empirical m a
smcFast = smcWithResampler resample

smcWithResampler :: MonadDist m =>
                    (forall x. Empirical m x -> Empirical m x) ->
                    Int -> Int -> Particle (Empirical m) a -> Empirical m a

smcWithResampler resampler k n =
  flatten . foldr (.) id (replicate k (advance . hoist' resampler)) . hoist' (spawn n >>)
  where
    hoist' = Particle.mapMonad

-- | Metropolis-Hastings kernel. Generates a new value and the MH ratio.
newtype MHKernel m a = MHKernel {runMHKernel :: a -> m (a,LogFloat)}

-- | Metropolis-Hastings algorithm.
mh :: MonadDist m => Int ->  Weighted m a -> MHKernel (Weighted m) a -> m [a]
mh n init trans = evalStateT (start >>= chain n) 1 where
  -- start :: StateT LogFloat m a
  start = do
    (x, p) <- lift $ runWeighted init
    if p == 0 then
      start
    else
      put p >> return x

  --chain :: Int -> a -> StateT LogFloat m [a]
  chain 0 _ = return []
  chain n x = do
    p <- get
    ((y,w), q) <- lift $ runWeighted $ runMHKernel trans x
    accept <- bernoulli $ if p == 0 then 1 else min 1 (q * w / p)
    let next = if accept then y else x
    when accept (put q)
    rest <- chain (n-1) next
    return (x:rest)

-- | Trace MH. Each state of the Markov chain consists of a list
-- of continuations from the sampling of each primitive distribution
-- during an execution.
traceMH :: (MonadDist m) => Weighted (Coprimitive m) a -> m [a]
traceMH m = mhState m >>= init >>= loop
  where
    init state | mhPosteriorWeight state >  0 = return state
    init state | mhPosteriorWeight state == 0 = mhState m >>= init
    loop state = do
      nextState <- mhKernel state
      otherAnswers <- loop nextState
      return (mhAnswer state : otherAnswers)

-- | Metropolis-Hastings version that uses the prior as proposal distribution.
mhPrior :: MonadDist m => Int -> Weighted m a -> m [a]
mhPrior n d = mh n d kernel where
    kernel = MHKernel $ const $ fmap (,1) d

-- | Particle Independent Metropolis Hastings. The first two arguments are
-- passed to SMC, the third is the number of samples, equal to
-- the number of SMC runs.
pimh :: MonadDist m => Int -> Int -> Int -> Particle (Empirical m) a -> m [a]
pimh k np ns d = mhPrior ns $ transform $ smc k np d
