{-# LANGUAGE
  FlexibleContexts,
  ScopedTypeVariables,
  Rank2Types,
  TupleSections
 #-}

module Inference where

import Control.Arrow (first,second)
import Data.Either
import Data.Number.LogFloat
import Data.Typeable
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Base
import Sampler
import Rejection
import Weighted
import Particle
import Empirical
import Dist
import Prior
import Trace

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
smc :: MonadDist m => Int -> ParticleT (EmpiricalT m) a -> EmpiricalT m a
smc n d = flatten $ run start where
    start = lift (spawn n) >> d
    step :: MonadDist m => ParticleT (EmpiricalT m) a -> ParticleT (EmpiricalT m) a
    step particles = mapMonad resample $ advance particles
    run :: MonadDist m => ParticleT (EmpiricalT m) a -> ParticleT (EmpiricalT m) a
    run particles = do
      finished <- lift $ lift $ Empirical.all id $ finished particles
      if finished then particles else run (step particles)

-- | `smc` with post-processing.
smc' :: (Ord a, Typeable a, MonadDist m) => Int ->
        ParticleT (EmpiricalT m) a -> m [(a,Double)]
smc' n d = fmap (enumerate . categorical) $ runEmpiricalT $ smc n d



-- | Metropolis-Hastings kernel. Generates a new value and the MH ratio.
newtype MHKernel m a = MHKernel {runMHKernel :: a -> m (a,LogFloat)}

mhKernel'  :: (RandomDB r, MonadDist m) => r -> UpdaterT r m a -> MHKernel m (a, r)
mhKernel' = const mhKernel

mhKernel :: (RandomDB r, MonadDist m) => UpdaterT r m a -> MHKernel m (a, r)
mhKernel program = MHKernel $ \(x, r) -> do
  r1 <- mutate r
  ((x', r'), leftover) <- runUpdaterT program r1
  return ((x', r'), mhCorrectionFactor r r')

-- | Metropolis-Hastings algorithm. The idea is that the kernel handles the
-- part of ratio that results from MonadDist effects and transition function,
-- while state carries the factor associated with MonadBayes effects.
mh :: MonadDist m => Int ->  WeightedT m a -> MHKernel (WeightedT m) a -> m [a]
mh n init trans = evalStateT (start >>= chain n) 1 where
  -- start :: StateT LogFloat m a
  start = do
    (x, p) <- lift $ runWeightedT init
    put p
    return x

  --chain :: Int -> StateT LogFloat m a -> StateT LogFloat m [a]
  chain 0 _ = return []
  chain n x = do
    p <- get
    ((y,w), q) <- lift $ runWeightedT $ runMHKernel trans x
    accept <- bernoulli $ if p == 0 then 1 else min 1 (q * w / p)
    let next = if accept then y else x
    when accept (put q)
    rest <- chain (n-1) next
    return (x:rest)

mh' :: (RandomDB r, MonadDist m) => r -> Int -> (forall m'. (MonadBayes m') => m' a) -> m [a]
mh' r0 n program = fmap (map fst) $ mh n (runTraceT program) $ mhKernel' r0 program

-- | Metropolis-Hastings version that uses the prior as proposal distribution.
mhPrior :: MonadDist m => Int -> WeightedT m a -> m [a]
mhPrior n d = mh n d kernel where
    kernel = MHKernel $ const $ fmap (,1) d

-- | Particle Independent Metropolis Hastings. The first argument is the number
-- of particles in each SMC run, the second is the number of samples, equal to
-- the number of SMC runs.
pimh :: MonadDist m => Int -> Int -> ParticleT (EmpiricalT m) a -> m [a]
pimh np ns d = mhPrior np $ transform $ smc np d
