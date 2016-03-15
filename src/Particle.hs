{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts,
  RankNTypes
 #-}

module Particle (
    ParticleT,
    synchronize,
    flatten,
    advance,
    finished,
    Particle.mapMonad
                ) where

import Control.Monad.Trans.Class
import Control.Monad (liftM2)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Either

import Base

-- | Particle represents a computation that can be paused at certain points.
-- The intermediate monadic effects can be extracted, which is particularly useful
-- for implementation of SMC-related methods.
-- All the probabilistic effects are delegated to the transformed monad,
-- but also `synchronize` is inserted after each `factor`.
type ParticleT m a = Coroutine (Await ()) m a
extract (Await f) = f ()

-- introduce a checkpoint
synchronize :: Monad m => ParticleT m ()
-- remove all remaining checkpoints
finish :: Monad m => ParticleT m a -> m a
-- move to the next checkpoint
advance :: Monad m =>
  ParticleT m a -> ParticleT m a
-- apply a function to the transformed monad
mapMonad :: Monad m =>
  (forall a. m a -> m a) ->
  ParticleT m a -> ParticleT m a

-- | A synchronization barrier where computation is paused.
synchronize :: Monad m => ParticleT m ()
synchronize = await

-- | Removes the synchronization barriers.
flatten :: Monad m => ParticleT m a -> m a
flatten = pogoStick extract

-- | Run a particle to the next barrier.
-- If the computation is finished do nothing.
advance :: Monad m => ParticleT m a -> ParticleT m a
advance = bounce extract

-- | Checks if the particle is finished.
finished :: Monad m => ParticleT m a -> m Bool
finished = fmap isRight . resume

mapMonad :: Monad m =>
            (forall a. m a -> m a) -> ParticleT m a -> ParticleT m a
mapMonad f cort = Coroutine {resume= f $ resume cort}

--instance MonadDist m => MonadDist (Coroutine (Await ()) m) where
instance MonadDist m =>
  MonadDist (ParticleT m) where
    primitive = lift . primitive

--instance MonadBayes m => MonadBayes (Coroutine (Await ()) m) where
instance MonadBayes m =>
  MonadBayes (ParticleT m) where
    factor w = lift (factor w) >> synchronize
