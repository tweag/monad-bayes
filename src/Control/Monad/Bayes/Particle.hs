{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts,
  RankNTypes
 #-}

module Control.Monad.Bayes.Particle (
    Particle,
    synchronize,
    flatten,
    advance,
    finished,
    mapMonad
                ) where

import Control.Monad.Trans.Class
import Control.Monad (liftM2)
import Control.Monad.Coroutine hiding (mapMonad)
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Either

import Control.Monad.Bayes.Class

-- | Particle represents a computation that can be paused at certain points.
-- The intermediate monadic effects can be extracted, which is particularly useful
-- for implementation of SMC-related methods.
-- All the probabilistic effects are delegated to the transformed monad,
-- but also `synchronize` is inserted after each `factor`.
type Particle m a = Coroutine (Await ()) m a
extract (Await f) = f ()

-- | A synchronization barrier where computation is paused.
synchronize :: Monad m => Particle m ()
synchronize = await

-- | Removes the synchronization barriers.
flatten :: Monad m => Particle m a -> m a
flatten = pogoStick extract

-- | Run a particle to the next barrier.
-- If the computation is finished do nothing.
advance :: Monad m => Particle m a -> Particle m a
advance = bounce extract

-- | Checks if the particle is finished.
finished :: Monad m => Particle m a -> m Bool
finished = fmap isRight . resume

mapMonad :: Monad m =>
            (forall a. m a -> m a) -> Particle m a -> Particle m a
mapMonad f cort = Coroutine {resume= f $ resume cort}

instance MonadDist m => MonadDist (Coroutine (Await ()) m) where
    primitive = lift . primitive

instance MonadBayes m => MonadBayes (Coroutine (Await ()) m) where
    factor w = lift (factor w) >> synchronize
