{-# LANGUAGE
  FlexibleContexts,
  ScopedTypeVariables,
  Rank2Types,
  TupleSections,
  GeneralizedNewtypeDeriving
 #-}

module Control.Monad.Bayes.Inference (
  rejection,
  importance,
  importance',
  smc,
  smc',
  smcWithResampler,
  smcrm,
  ismh,
  smh,
  traceMH,
  mhPrior,
  pimh
) where

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

-- | Rejection sampling that proposes from the prior.
-- The accept/reject decision is made for the whole program rather than
-- individual variables.
-- The program must not contain factors larger than 1.
rejection :: MonadDist m => Int -- ^ number of samples accepted
                         -> Rejection m a -> m [a]
rejection n d = sequence $ replicate n $ sample where
  sample = do
    m <- runRejection d
    case m of Just x  -> return x
              Nothing -> sample

-- | Simple importance sampling from the prior.
importance :: MonadDist m
              => Int -- ^ numer of samples produced
              -> Population m a -> m [(a, CustomReal m)]
importance n = fmap (map (second fromLogDomain)) . runPopulation . (spawn n >>)

-- | Multiple importance samples with post-processing that aggregates weights of equal elements.
-- It does not normalize the weights.
importance' :: (Ord a, MonadDist m) =>
               Int -> Population m a -> m [(a, CustomReal m)]
importance' n d = fmap compact $ importance n d

-- | Sequential Monte Carlo from the prior.
smc :: MonadDist m => Int -- ^ number of resampling points
                   -> Int -- ^ number of particles
                   -> Particle (Population m) a -> Population m a
smc k n = smcWithResampler (resampleN n) k n

-- | `smc` with post-processing like in 'importance''.
smc' :: (Ord a, MonadDist m) => Int -> Int ->
        Particle (Population m) a -> m [(a, CustomReal m)]
smc' k n d = fmap (compact . map (second fromLogDomain)) $ runPopulation $ smc k n d

-- | Asymptotically faster version of 'smc' that resamples using multinomial
-- instead of a sequence of categoricals.
-- smcFast :: MonadDist m => Int -> Int -> Particle (Population m) a -> Population m a
-- smcFast = smcWithResampler resample

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

-- | Like `smc`, but with a custom resampling scheme.
smcWithResampler :: MonadDist m =>
  (forall x. Population m x -> Population m x) -- ^ resampling function
  -> Int -- ^ number of resampling points
  -> Int -- ^ number of particles
  -> Particle (Population m) a -> Population m a

smcWithResampler resampler k n =
  flatten . composeCopies k (advance . hoist' resampler) . hoist' (spawn n >>)
  where
    hoist' = Particle.mapMonad

-- | Resample-move Sequential Monte Carlo algorithm.
-- Rejuvenates particles with a single step of Lightweight Metropolis-Hastings
-- after each resampling point.
smcrm :: forall m a. MonadDist m =>
         Int -- ^ number of resampling points
         -> Int -- ^ number of MH transitions at each step
         -> Int -- ^ number of particles
         -> Particle (Trace (Population m)) a -> Population m a

smcrm k s n = marginal . flatten . composeCopies k step . init
  where
  hoistC  = Particle.mapMonad
  hoistT  = Trace.mapMonad

  init :: Particle (Trace (Population m)) a -> Particle (Trace (Population m)) a
  init = hoistC (hoistT (spawn n >>))

  step :: Particle (Trace (Population m)) a -> Particle (Trace (Population m)) a
  step = advance . hoistC (composeCopies s mhStep . hoistT resample)

-- | Importance Sampling with Metropolis-Hastings transitions.
-- Draws initial samples using IS and applies a number of MH transitions
-- to each of them independently.
-- Can be seen as a precursor to Simulated Annealing.
ismh :: MonadDist m => Int -- ^ number of MH transitions for each point
                    -> Int -- ^ population size
                    -> Trace (Population m) a -> Population m a
ismh s n = marginal . composeCopies s mhStep . Trace.mapMonad (spawn n >>)

-- | Sequential Metropolis-Hastings.
-- Alternates several MH transitions with running the program another step forward.
smh :: MonadBayes m => Int -- ^ number of suspension points
                    -> Int -- ^ number of MH transitions at each point
                    -> Particle (Trace m) a -> m a
smh k s = marginal . flatten . composeCopies k (advance . composeCopies s (Particle.mapMonad mhStep))

-- | Metropolis-Hastings kernel. Generates a new value and the MH ratio.
newtype MHKernel m a = MHKernel {runMHKernel :: a -> m (a, LogDomain (CustomReal m))}

-- | Generic Metropolis-Hastings algorithm.
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

-- | Lightweight Metropolis-Hastings.
-- The first sample is drawn from the prior, so the number of MH transitions
-- is one less than the number of samples.
-- Beware that if the initial sample has zero likelihood, it is possible
-- that all the samples produced have zero likelihood.
traceMH :: MonadDist m => Int -- ^ number of samples produced
                       -> Trace (WriterT [a] (Prior m)) a -> m [a]
traceMH n m = prior $ execWriterT $ marginal $ composeCopies (n-1) mhStep $ record m where
  record m = do
    x <- m
    lift (tell [x])
    return x

-- | Metropolis-Hastings version that uses the prior as proposal distribution.
mhPrior :: MonadDist m => Int -> Weighted m a -> m [a]
mhPrior n d = mh n d kernel where
    kernel = MHKernel $ const $ fmap (,1) d

-- | Particle Independent Metropolis Hastings.
-- Outputs one sample per SMC run.
pimh :: MonadDist m => Int -- ^ number of resampling points in SMC
                    -> Int -- ^ number of particles in SMC
                    -> Int -- ^ number of independent SMC runs
                    -> Particle (Population (Weighted m)) a -> m [a]
pimh k np ns d = mhPrior ns $ collapse $ smc k np d
