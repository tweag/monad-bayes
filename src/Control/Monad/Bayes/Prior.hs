{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  TypeFamilies
 #-}

module Control.Monad.Bayes.Prior (
    Prior,
    prior
              ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity

import Control.Monad.Bayes.Class

-- | A simple wrapper around 'MonadDist' types that discards conditoning.
newtype Prior m a = Prior {runPrior :: IdentityT m a}
    deriving(Functor, Applicative, Monad, MonadTrans)

type instance CustomReal (Prior m) = CustomReal m

instance MonadDist m => MonadDist (Prior m) where
  primitive = lift . primitive

instance MonadDist m => MonadBayes (Prior m) where
    factor _ = return ()

-- | Sampling from the prior discarding conditioning.
prior :: Prior m a -> m a
prior = runIdentityT . runPrior
