{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts,
  RankNTypes,
  TypeFamilies
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
-- All the probabilistic effects are lifted from the transformed monad,
-- but also `synchronize` is inserted after each `factor`.
newtype Particle m a = Particle {runParticle :: (Coroutine (Await ()) m a)}
  deriving(Functor,Applicative,Monad,MonadTrans)
extract (Await f) = f ()

-- | A synchronization barrier where computation is paused.
synchronize :: Monad m => Particle m ()
synchronize = Particle await

-- | Remove the synchronization barriers.
flatten :: Monad m => Particle m a -> m a
flatten = pogoStick extract . runParticle

-- | Run a particle to the next barrier.
-- If the computation is finished do nothing.
--
-- > flatten = flatten . advance
advance :: Monad m => Particle m a -> Particle m a
advance = Particle . bounce extract . runParticle

-- | Checks if the particle is finished.
finished :: Monad m => Particle m a -> m Bool
finished = fmap isRight . resume . runParticle

-- | Modify the transformed monad.
-- This operation only applies to computation up to the first barrier.
mapMonad :: Monad m =>
            (forall a. m a -> m a) -> Particle m a -> Particle m a
mapMonad f = Particle . Coroutine . f . resume . runParticle

type instance CustomReal (Particle m) = CustomReal m

instance MonadDist m => MonadDist (Particle m) where
  primitive = lift . primitive

instance MonadBayes m => MonadBayes (Particle m) where
  factor w = lift (factor w) >> synchronize
