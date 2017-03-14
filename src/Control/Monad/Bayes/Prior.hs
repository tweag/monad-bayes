{-|
Module      : Control.Monad.Bayes.Prior
Description : Probability monad that ignores conditioning
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Prior (
    Prior,
    prior,
    hoist
              ) where

import Control.Monad.Trans
import Control.Monad.Trans.Identity

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple

-- | A simple wrapper around 'MonadDist' types that discards conditoning.
newtype Prior m a = Prior {runPrior :: IdentityT m a}
    deriving(Functor, Applicative, Monad, MonadTrans, MonadIO)

instance HasCustomReal m => HasCustomReal (Prior m) where
  type CustomReal (Prior m) = CustomReal m

instance (Sampleable d m, Monad m) => Sampleable d (Prior m) where
  sample = lift . sample

instance (Applicative m, HasCustomReal m) => Conditionable (Prior m) where
  factor _ = pure ()

instance MonadDist m => MonadDist (Prior m)
instance MonadDist m => MonadBayes (Prior m)

-- | Discard conditioning and just use the prior.
prior :: Prior m a -> m a
prior = runIdentityT . runPrior

-- | Apply a transformation to the inner monad.
hoist :: (forall x. m x -> n x) -> Prior m a -> Prior n a
hoist f = Prior . IdentityT . f . runIdentityT . runPrior
