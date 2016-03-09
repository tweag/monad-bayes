{-# LANGUAGE
  FlexibleContexts
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
importance' n d = fmap (enumerate . categorical) $ runEmpiricalT $ population n >> d

-- | Sequential Monte Carlo from the prior.
smc :: MonadDist m => Int -> ParticleT (EmpiricalT m) a -> EmpiricalT m a
smc n d = flatten $ run start where
    start = lift (population n) >> d
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

-- | Metropolis-Hastings algorithm. The idea is that the kernel handles the
-- part of ratio that results from MonadDist effects and transition function,
-- while state carries the factor associated with MonadBayes effects.
mh :: MonadDist m => Int ->  WeightedT m a -> MHKernel (WeightedT m) a -> m [a]
mh n init trans = evalStateT (start >>= chain n) 1 where
  --start :: StateT LogFloat m a
  start = do
    (x, p) <- lift $ runWeightedT init
    put p
    return x

  --chain :: Int -> StateT LogFloat m a -> StateT LogFloat m [a]
  chain 0 _ = return []
  chain n x = do
    p <- get
    ((y,w), q) <- lift $ runWeightedT $ runMHKernel trans x
    accept <- bernoulli $ min 1 (q * w / p)
    let next = if accept then y else x
    when accept (put q)
    rest <- chain (n-1) next
    return (x:rest)
