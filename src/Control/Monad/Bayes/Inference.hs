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
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Control.Monad.Bayes.LogDomain (LogDomain, fromLogDomain, toLogDomain)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Rejection
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Particle as Particle
import Control.Monad.Bayes.Trace    as Trace
import Control.Monad.Bayes.Empirical
import Control.Monad.Bayes.Dist
import Control.Monad.Bayes.Prior

-- | Rejection sampling.
rejection :: MonadDist m => Rejection m a -> m a
rejection d = do
  m <- runRejection d
  case m of Just x  -> return x
            Nothing -> rejection d

-- | Simple importance sampling from the prior.
importance :: MonadDist m => Weighted m a -> m (a,LogDomain (CustomReal m))
importance = runWeighted

-- | Multiple importance samples with post-processing.
importance' :: (Ord a, MonadDist m) =>
               Int -> Population m a -> m [(a, CustomReal m)]
importance' n d = fmap (compact . map (second fromLogDomain)) $ runPopulation $ spawn n >> d

-- | Sequential Monte Carlo from the prior.
-- The first argument is the number of resampling points, the second is
-- the number of particles used.
-- If the first argument is smaller than the number of observations in the model,
-- the algorithm is still correct, but doesn't perform resampling after kth time.
smc :: MonadDist m => Int -> Int -> Particle (Population m) a -> Population m a
smc k n = smcWithResampler (resampleN n) k n

-- | `smc` with post-processing.
smc' :: (Ord a, MonadDist m) => Int -> Int ->
        Particle (Population m) a -> m [(a, CustomReal m)]
smc' k n d = fmap (compact . map (second fromLogDomain)) $ runPopulation $ smc k n d

-- | Asymptotically faster version of 'smc' that resamples using multinomial
-- instead of a sequence of categoricals.
smcFast :: MonadDist m => Int -> Int -> Particle (Population m) a -> Population m a
smcFast = smcWithResampler resample

composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

smcWithResampler :: MonadDist m =>
                    (forall x. Population m x -> Population m x) ->
                    Int -> Int -> Particle (Population m) a -> Population m a

smcWithResampler resampler k n =
  flatten . composeCopies k (advance . hoist' resampler) . hoist' (spawn n >>)
  where
    hoist' = Particle.mapMonad

smcrm :: forall m a. MonadDist m =>
         Int -> Int ->
         Particle (Trace (Population m)) a -> Population m a

smcrm k n = marginal . flatten . composeCopies k step . init
  where
  hoistC  = Particle.mapMonad
  hoistT  = Trace.mapMonad

  init :: Particle (Trace (Population m)) a -> Particle (Trace (Population m)) a
  init = hoistC (hoistT (spawn n >>))

  step :: Particle (Trace (Population m)) a -> Particle (Trace (Population m)) a
  step = advance . hoistC (mhStep . hoistT resample)

-- | Importance Sampling with Metropolis-Hastings transitions.
-- Draws initial samples using IS and applies a number of MH transitions
-- to each of them independently.
-- Can be seen as a precursor to Simulated Annealing.
ismh :: MonadDist m => Int -> Int -> Trace (Population m) a -> Population m a
ismh s n = marginal . composeCopies s mhStep . Trace.mapMonad (spawn n >>)

-- | Sequential Metropolis-Hastings.
-- Alternates several MH transitions with running the program another step forward.
smh :: MonadBayes m => Int -> Int -> Particle (Trace m) a -> m a
smh k s = marginal . flatten . composeCopies k (advance . composeCopies s (Particle.mapMonad mhStep))

-- | Metropolis-Hastings kernel. Generates a new value and the MH ratio.
newtype MHKernel m a = MHKernel {runMHKernel :: a -> m (a, LogDomain (CustomReal m))}

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
    accept <- bernoulli $ if p == 0 then 1 else min 1 $ fromLogDomain (q * w / p)
    let next = if accept then y else x
    when accept (put q)
    rest <- chain (n-1) next
    return (x:rest)

-- | Trace MH. Each state of the Markov chain consists of a list
-- of continuations from the sampling of each primitive distribution
-- during an execution.
traceMH :: (MonadDist m) => Int -> Trace' m a -> m [a]
traceMH n (Trace' m) = m 1 >>= init >>= loop n
  where
    init state | mhPosteriorWeight state >  0 = return state
    init state | mhPosteriorWeight state == 0 = m 1 >>= init
    loop n state | n <= 0 = return []
    loop n state | n >  0 = do
      nextState <- mhKernel state
      otherAnswers <- loop (n - 1) nextState
      return (mhAnswer state : otherAnswers)

-- | Metropolis-Hastings version that uses the prior as proposal distribution.
mhPrior :: MonadDist m => Int -> Weighted m a -> m [a]
mhPrior n d = mh n d kernel where
    kernel = MHKernel $ const $ fmap (,1) d

-- | Particle Independent Metropolis Hastings. The first two arguments are
-- passed to SMC, the third is the number of samples, equal to
-- the number of SMC runs.
pimh :: MonadDist m => Int -> Int -> Int -> Particle (Population (Weighted m)) a -> m [a]
pimh k np ns d = mhPrior ns $ collapse $ smc k np d
