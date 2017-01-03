{-|
Module      : Control.Monad.Bayes.Inference
Description : Inference algorithms for probabilistic programs
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  ScopedTypeVariables
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
  pimh,
  smcHerdingResample
) where

import Control.Arrow (second)
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import qualified Numeric.LinearAlgebra as LA

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Rejection
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sequential as Sequential
import Control.Monad.Bayes.Trace    as Trace
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Prior
import Control.Monad.Bayes.Augmented as Augmented
import Control.Monad.Bayes.Herding

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
                   -> Sequential (Population m) a -> Population m a
smc k n = smcWithResampler resample k n

-- | `smc` with post-processing like in 'importance''.
smc' :: (Ord a, MonadDist m) => Int -> Int ->
        Sequential (Population m) a -> m [(a, CustomReal m)]
smc' k n d = fmap (compact . map (second fromLogDomain)) $ runPopulation $ smc k n d

-- | Asymptotically faster version of 'smc' that resamples using multinomial
-- instead of a sequence of categoricals.
-- smcFast :: MonadDist m => Int -> Int -> Sequential (Population m) a -> Population m a
-- smcFast = smcWithResampler resample

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

-- | Like `smc`, but with a custom resampling scheme.
smcWithResampler :: MonadDist m =>
  (forall x. Population m x -> Population m x) -- ^ resampling function
  -> Int -- ^ number of resampling points
  -> Int -- ^ number of particles
  -> Sequential (Population m) a -> Population m a

smcWithResampler resampler k n =
  finish . composeCopies k (advance . hoist' resampler) . hoist' (spawn n >>)
  where
    hoist' = Sequential.hoistFirst

-- | Resample-move Sequential Monte Carlo algorithm.
-- Rejuvenates particles with a single step of Lightweight Metropolis-Hastings
-- after each resampling point.
smcrm :: forall m a. MonadDist m =>
         Int -- ^ number of resampling points
         -> Int -- ^ number of MH transitions at each step
         -> Int -- ^ number of particles
         -> Sequential (Traced (Population m)) a -> Population m a

smcrm k s n = dropTrace . finish . composeCopies k step . start
  where
  hoistC  = Sequential.hoistFirst
  hoistT  = Trace.hoist

  start :: Sequential (Traced (Population m)) a -> Sequential (Traced (Population m)) a
  start = hoistC (hoistT (spawn n >>))

  step :: Sequential (Traced (Population m)) a -> Sequential (Traced (Population m)) a
  step = advance . hoistC (composeCopies s mhStep . hoistT resample)

-- | Importance Sampling with Metropolis-Hastings transitions.
-- Draws initial samples using IS and applies a number of MH transitions
-- to each of them independently.
-- Can be seen as a precursor to Simulated Annealing.
ismh :: MonadDist m => Int -- ^ number of MH transitions for each point
                    -> Int -- ^ population size
                    -> Traced (Population m) a -> Population m a
ismh s n = dropTrace . composeCopies s mhStep . Trace.hoist (spawn n >>)

-- | Sequential Metropolis-Hastings.
-- Alternates several MH transitions with running the program another step forward.
smh :: MonadBayes m => Int -- ^ number of suspension points
                    -> Int -- ^ number of MH transitions at each point
                    -> Sequential (Traced m) a -> m a
smh k s = dropTrace . finish . composeCopies k (advance . composeCopies s (Sequential.hoistFirst mhStep))

-- | Metropolis-Hastings kernel. Generates a new value and the MH ratio.
newtype MHKernel m a = MHKernel {runMHKernel :: a -> m (a, LogDomain (CustomReal m))}

-- | Generic Metropolis-Hastings algorithm.
mh :: MonadDist m => Int ->  Weighted m a -> MHKernel (Weighted m) a -> m [a]
mh n initial trans = evalStateT (start >>= chain n) 1 where
  -- start :: StateT LogFloat m a
  start = do
    (x, p) <- lift $ runWeighted initial
    if p == 0 then
      start
    else
      put p >> return x

  --chain :: Int -> a -> StateT LogFloat m [a]
  chain 0 _ = return []
  chain k x = do
    p <- get
    ((y,w), q) <- lift $ runWeighted $ runMHKernel trans x
    accept <- bernoulli $ if p == 0 then 1 else min 1 $ fromLogDomain (q * w / p)
    let next = if accept then y else x
    when accept (put q)
    rest <- chain (k-1) next
    return (x:rest)

-- | Lightweight Metropolis-Hastings.
-- The first sample is drawn from the prior, so the number of MH transitions
-- is one less than the number of samples.
-- Beware that if the initial sample has zero likelihood, it is possible
-- that all the samples produced have zero likelihood.
traceMH :: MonadDist m => Int -- ^ number of samples produced
                       -> Traced (WriterT [a] (Prior m)) a -> m [a]
traceMH n m = prior $ execWriterT $ dropTrace $ composeCopies (n-1) mhStep $ record m where
  record d = do
    x <- d
    lift (tell [x])
    return x

-- | Metropolis-Hastings version that uses the prior as proposal distribution.
mhPrior :: MonadDist m => Int -> Weighted m a -> m [a]
mhPrior n d = mh n d kernel where
    kernel = MHKernel $ const $ fmap (,1) d

-- | Sequential Independent Metropolis Hastings.
-- Outputs one sample per SMC run.
pimh :: MonadDist m => Int -- ^ number of resampling points in SMC
                    -> Int -- ^ number of particles in SMC
                    -> Int -- ^ number of independent SMC runs
                    -> Sequential (Population (Weighted m)) a -> m [a]
pimh k np ns d = mhPrior ns $ collapse $ smc k np d

smcHerdingResample :: (MonadDist m, CustomReal m ~ Double)
                   => Kernel Double (Trace Double)
                   -> Int
                   -> Int
                   -> Sequential (Augmented (Population m)) a
                   -> Population m a
smcHerdingResample kernel k n = marginal . finish . composeCopies k step . start where
  hoistC  = Sequential.hoistFirst
  hoistA  = Augmented.hoist
  start = hoistC (hoistA (spawn n >>))
  step = advance . hoistC (herdingTraceResample kernel)

herdingTraceResample :: (MonadDist m, CustomReal m ~ Double)
                     => Kernel Double (Trace Double)
                     -> Augmented (Population m) a -> Augmented (Population m) a
herdingTraceResample k m = Augmented.hoist (herdingResample k') m where
  k' = compose k snd

herdingResample :: (MonadDist m, CustomReal m ~ Double)
                => Kernel (CustomReal m) a -> Population m a -> Population m a
herdingResample kernel pop = mapPopulation f pop where
  f ps = return $ map (, logZ / fromIntegral n) ys where
    (xs, logWs) = unzip ps
    logZ = sum logWs
    ws = LA.vector $ map (fromLogDomain . subtract logZ) logWs
    emb = (kernel, ws, xs)
    n = length xs
    ys = herding emb n
