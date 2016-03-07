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
importance :: WriterT (Product LogFloat) Sampler a -> Sampler (a,LogFloat)
importance d = fmap (second getProduct) (runWriterT d)

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
mh :: (MonadWriter (Product LogFloat) m, MonadDist m) => Int ->  m a -> MHKernel m a -> m [a]
mh n init trans = censor (const 1) (evalStateT (start >>= chain n) 1) where
  --start :: StateT LogFloat m a
  start = do
    (x, Product p) <- lift $ listen init
    put p
    return x

  --chain :: Int -> StateT LogFloat m a -> StateT LogFloat m [a]
  chain 0 _ = return []
  chain n x = do
    p <- get
    ((y,w), Product q) <- lift $ listen $ runMHKernel trans x
    accept <- bernoulli $ min 1 (q * w / p)
    let next = if accept then y else x
    when accept (put q)
    rest <- chain (n-1) next
    return (x:rest)
