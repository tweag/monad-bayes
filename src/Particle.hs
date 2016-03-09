{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts
 #-}

module Particle (
    ParticleT,
    particleT,
    runParticleT,
    synchronize,
    flatten,
    advance,
    mapMonad
                ) where

import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad (liftM2)
import Control.Monad.Coroutine

import Base

-- | Particle represents a computation that can be paused at certain points.
-- The intermediate monadic effects can be extracted, which is particularly useful
-- for implementation of SMC-related methods.
-- All the probabilistic effects are delegated to the transformed monad,
-- but also `synchronize` is inserted after each `factor`.
type ParticleT m a = Coroutine Identity m a
particleT = Coroutine
runParticleT = resume

-- | A synchronization barrier where computation is paused.
synchronize :: Monad m => ParticleT m ()
synchronize = suspend $ Identity $ return ()

-- | Removes the synchronization barriers.
flatten :: Monad m => ParticleT m a -> m a
flatten = pogoStick runIdentity

instance MonadDist m => MonadDist (Coroutine Identity m) where
    primitive = lift . primitive

instance MonadBayes m => MonadBayes (Coroutine Identity m) where
    factor w = lift (factor w) >> synchronize

-- | Run a particle to the next barrier.
-- If the computation is finished do nothing.
advance :: Monad m => ParticleT m a -> ParticleT m a --m (CoroutineStepResult Identity m a)
advance = bounce runIdentity
