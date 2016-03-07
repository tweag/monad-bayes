{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts
 #-}

module Particle (
    ParticleT,
    runParticleT,
    synchronize,
    flatten,
    advance
                ) where

import Control.Monad.Trans.Class
import Control.Monad (liftM2)

import Base

-- | Particle represents a computation that can be paused at certain points.
-- The intermediate monadic effects can be extracted, which is particularly useful
-- for implementation of SMC-related methods.
-- All the probabilistic effects are delegated to the transformed monad,
-- but also `synchronize` is inserted after each `factor`.
newtype ParticleT m a = ParticleT {runParticleT :: m (Either a (ParticleT m a))}

-- | A synchronization barrier where computation is paused.
synchronize :: Monad m => ParticleT m ()
synchronize = ParticleT $ return $ Right $ return ()

-- | Removes the synchronization barriers.
flatten :: Monad m => ParticleT m a -> m a
flatten (ParticleT m) = do
  e <- m
  case e of Left x  -> return x
            Right p -> flatten p

instance Functor m => Functor (ParticleT m) where
    fmap f (ParticleT d) = ParticleT $ fmap (emap f) d where
                                            emap f (Left x) = Left (f x)
                                            emap f (Right d) = Right (fmap f d)

instance Monad m => Applicative (ParticleT m) where
    pure = return
    (<*>) = liftM2 ($)

instance Monad m => Monad (ParticleT m) where
    return x = ParticleT $ return $ Left x
    ParticleT d >>= f =
        ParticleT $ do
          p <- d
          case p of Left x  -> runParticleT (f x)
                    Right q -> return $ Right $ q >>= f
    fail = lift . fail

instance MonadTrans ParticleT where
    lift = ParticleT . fmap Left

instance MonadDist m => MonadDist (ParticleT m) where
    categorical = lift . categorical
    normal m s  = lift (normal m s)
    gamma a b   = lift (gamma a b)
    beta a b    = lift (beta a b)

instance MonadBayes m => MonadBayes (ParticleT m) where
    factor w = lift (factor w) >> synchronize

-- | Run a particle to the next barrier.
-- If the computation is finished do nothing.
advance :: Monad m => Either a (ParticleT m a) -> m (Either a (ParticleT m a))
advance (Left x)  = return (Left x)
advance (Right p) = runParticleT p
