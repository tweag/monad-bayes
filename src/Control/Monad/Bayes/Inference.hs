{-# LANGUAGE
  FlexibleContexts,
  ScopedTypeVariables,
  Rank2Types,
  TupleSections
 #-}

module Control.Monad.Bayes.Inference where

import Control.Arrow (first,second)
import Data.Either
import Data.Number.LogFloat
import Data.Typeable
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Rejection
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Particle as Particle
import Control.Monad.Bayes.Empirical
import Control.Monad.Bayes.Dist
import Control.Monad.Bayes.Prior
import Control.Monad.Bayes.Trace

-- | Rejection sampling.
rejection :: MonadDist m => RejectionT m a -> m a
rejection d = do
  m <- runRejectionT d
  case m of Just x  -> return x
            Nothing -> rejection d

-- | Simple importance sampling from the prior.
importance :: MonadDist m => WeightedT m a -> m (a,LogFloat)
importance = runWeightedT

-- | Multiple importance samples with post-processing.
importance' :: (Ord a, Typeable a, MonadDist m) =>
               Int -> EmpiricalT m a -> m [(a,Double)]
importance' n d = fmap (enumerate . categorical) $ runEmpiricalT $ spawn n >> d

-- | Sequential Monte Carlo from the prior.
-- The first argument is the number of resampling points, the second is
-- the number of particles used.
-- If the first argument is smaller than the number of observations in the model,
-- the algorithm is still correct, but doesn't perform resampling after kth time.
smc :: MonadDist m => Int -> Int -> ParticleT (EmpiricalT m) a -> EmpiricalT m a
smc k n d = flatten $ foldr (.) id (replicate k step) $ start where
  start = lift (spawn n) >> d
  step = Particle.mapMonad (resampleN n) . advance

-- | `smc` with post-processing.
smc' :: (Ord a, Typeable a, MonadDist m) => Int -> Int ->
        ParticleT (EmpiricalT m) a -> m [(a,Double)]
smc' k n d = fmap (enumerate . categorical) $ runEmpiricalT $ smc k n d

-- | Asymptotically faster version of 'smc' that resamples using multinomial
-- instead of a sequence of categoricals.
smcFast :: MonadDist m => Int -> Int -> ParticleT (EmpiricalT m) a -> EmpiricalT m a
smcFast k n d = flatten $ foldr (.) id (replicate k step) $ start where
  start = lift (spawn n) >> d
  step = Particle.mapMonad resample . advance

-- | Metropolis-Hastings kernel. Generates a new value and the MH ratio.
newtype MHKernel m a = MHKernel {runMHKernel :: a -> m (a,LogFloat)}

-- | Metropolis-Hastings algorithm.
mh :: MonadDist m => Int ->  WeightedT m a -> MHKernel (WeightedT m) a -> m [a]
mh n init trans = evalStateT (start >>= chain n) 1 where
  -- start :: StateT LogFloat m a
  start = do
    (x, p) <- lift $ runWeightedT init
    if p == 0 then
      start
    else
      put p >> return x

  --chain :: Int -> a -> StateT LogFloat m [a]
  chain 0 _ = return []
  chain n x = do
    p <- get
    ((y,w), q) <- lift $ runWeightedT $ runMHKernel trans x
    accept <- bernoulli $ if p == 0 then 1 else min 1 (q * w / p)
    let next = if accept then y else x
    when accept (put q)
    rest <- chain (n-1) next
    return (x:rest)

-- | Trace MH. Each state of the Markov chain consists of a list
-- of continuations from the sampling of each primitive distribution
-- during an execution.
traceMH :: (MonadDist m) => WeightedT (Coprimitive m) a -> m [a]
traceMH m = mhState m >>= init >>= loop
  where
    init state | mhPosteriorWeight state >  0 = return state
    init state | mhPosteriorWeight state == 0 = mhKernel state >>= init
    loop state = do
      nextState <- mhKernel state
      otherAnswers <- loop nextState
      return (mhAnswer state : otherAnswers)

-- | Metropolis-Hastings version that uses the prior as proposal distribution.
mhPrior :: MonadDist m => Int -> WeightedT m a -> m [a]
mhPrior n d = mh n d kernel where
    kernel = MHKernel $ const $ fmap (,1) d

-- | Particle Independent Metropolis Hastings. The first two arguments are
-- passed to SMC, the third is the number of samples, equal to
-- the number of SMC runs.
pimh :: MonadDist m => Int -> Int -> Int -> ParticleT (EmpiricalT m) a -> m [a]
pimh k np ns d = mhPrior ns $ transform $ smc k np d
