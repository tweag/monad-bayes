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

-- | Discard conditioning and just use the prior.
prior :: Prior m a -> m a
prior = runIdentityT . runPrior

-- | Apply a transformation to the inner monad.
hoist :: (forall x. m x -> n x) -> Prior m a -> Prior n a
hoist f = Prior . IdentityT . f . runIdentityT . runPrior
