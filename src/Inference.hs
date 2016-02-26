
module Inference where

import Data.Either
import Data.Number.LogFloat
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
importance' :: Ord a => Int -> EmpiricalT Sampler a -> Sampler [(a,Double)]
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
smc' :: Ord a => Int -> ParticleT (EmpiricalT Sampler) a -> Sampler [(a,Double)]
smc' n d = fmap (enumerate . categorical) $ toCat $ smc n d
