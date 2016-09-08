{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  TypeFamilies
 #-}

module Control.Monad.Bayes.Rejection (
                  Rejection,
                  runRejection) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Control.Monad.Bayes.Class

-- | A probability monad that aborts when a condition is violated.
-- Only hard conditioning is allowed, that is
-- 'condition' works correctly, while 'factor' and 'observe'
-- result in an error.
newtype Rejection m a = Rejection {toMaybeT :: (MaybeT m a)}
    deriving (Functor, Applicative, Monad, MonadTrans)

-- | Run the probabilistic computation and produce a result if all conditions
-- are satisfied.
runRejection :: Rejection m a -> m (Maybe a)
runRejection = runMaybeT . toMaybeT

type instance CustomReal (Rejection m) = CustomReal m

instance MonadDist m => MonadDist (Rejection m) where
  primitive = lift . primitive

instance MonadDist m => MonadBayes (Rejection m) where
  factor _ = error "Rejection does not support soft conditioning"
  condition b = unless b (fail "")
