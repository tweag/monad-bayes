{-# LANGUAGE
  FlexibleContexts
 #-}

module Inference where

import Data.Either
import Data.Number.LogFloat
import Data.Typeable
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy

import Base
import Sampler
import Particle
import Empirical
import Dist

-- | Rejection sampling.
rejection :: MaybeT Sampler a -> Sampler a
rejection d = do
  m <- runMaybeT d
  case m of Just x  -> return x
            Nothing -> rejection d

-- | Simple importance sampling from the prior.
importance :: StateT LogFloat Sampler a -> Sampler (a,LogFloat)
importance d = runStateT d 1

-- | Multiple importance samples with post-processing.
importance' :: (Ord a, Typeable a) => Int -> EmpiricalT Sampler a -> Sampler [(a,Double)]
importance' n d = fmap (enumerate . categorical) $ toCat $ population n >> d

-- | Sequential Monte Carlo from the prior.
smc :: Int -> ParticleT (EmpiricalT Sampler) a -> EmpiricalT Sampler a
smc n d = fmap (\(Left x) -> x) $ run start where
    ParticleT start = lift (population n) >> d
    step particles = resample $ particles >>= advance
    run particles = do
      finished <- lift $ Empirical.all isLeft particles
      if finished then particles else run (step particles)

-- | `smc` with post-processing.
smc' :: (Ord a, Typeable a) => Int -> ParticleT (EmpiricalT Sampler) a -> Sampler [(a,Double)]
smc' n d = fmap (enumerate . categorical) $ toCat $ smc n d



-- | Metropolis-Hastings kernel. Generates a new value and the MH ratio.
newtype MHKernel m a = MHKernel {runMHKernel :: a -> m (a,LogFloat)}

-- | Metropolis-Hastings algorithm. The idea is that the kernel handles the
-- part of ratio that results from MonadDist effects and transition function,
-- while state carries the factor associated with MonadBayes effects.
mh :: (MonadState LogFloat m, MonadDist m) => m a -> MHKernel m a -> m [a]
mh init trans = put 1 >> init >>= chain where
  chain x = do
    p <- get
    put 1
    (y,w) <- runMHKernel trans x
    q <- get
    accept <- bernoulli $ min 1 (q * w / p)
    let next = if accept then y else x
    unless accept (put p)
    rest <- chain next
    return (x:rest)
