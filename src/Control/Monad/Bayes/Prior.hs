{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  TypeFamilies
 #-}

module Control.Monad.Bayes.Prior (
    PriorT,
    prior
              ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity

import Control.Monad.Bayes.Class

-- | A simple wrapper around 'MonadDist' types that discards conditoning.
newtype PriorT m a = PriorT {runPriorT :: IdentityT m a}
    deriving(Functor, Applicative, Monad, MonadTrans)

type instance CustomReal (PriorT m) = CustomReal m

instance MonadDist m => MonadDist (PriorT m) where
  primitive = lift . primitive

instance MonadDist m => MonadBayes (PriorT m) where
    factor _ = return ()

-- | Sampling from the prior discarding conditioning.
prior :: PriorT m a -> m a
prior = runIdentityT . runPriorT
