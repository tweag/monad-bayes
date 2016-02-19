
module Inference where

import Data.Either
import Data.Number.LogFloat
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy

import Sampler
import Particle
import Empirical

-- | Rejection sampling.
rejection :: MaybeT Sampler a -> Sampler a
rejection d = do
  m <- runMaybeT d
  case m of Just x  -> return x
            Nothing -> rejection d

-- | Simple importance sampling from the prior.
importance :: StateT LogFloat Sampler a -> Sampler (a,LogFloat)
importance d = runStateT d 1

-- | Sequential Monte Carlo from the prior.
smc :: Int -> ParticleT (EmpiricalT Sampler) a -> EmpiricalT Sampler a
smc n d =
    let
        ParticleT start = lift (population n) >> d
        step particles = resample $ particles >>= advance
        resample = undefined
        run particles = do
          finished <- lift $ Empirical.all isLeft particles
          if finished then particles else run (step particles)
    in
      fmap (\(Left x) -> x) $ run start
