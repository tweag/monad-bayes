{-# LANGUAGE
  GeneralizedNewtypeDeriving
 #-}

module Rejection (
                  RejectionT,
                  runRejectionT) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Base

-- | A wrapper for 'MaybeT' that uses it for conditioning.
-- Only hard conditioning is allowed, that is
-- 'condition' works correctly, while 'factor' and'observe'
-- result in an error.
newtype RejectionT m a = RejectionT
  {toMaybeT :: (MaybeT m a)}
--    deriving (Monad, MonadTrans, MonadDist)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadDist)

-- | Equivalent to 'runMaybeT'
runRejectionT :: RejectionT m a -> m (Maybe a)
runRejectionT = runMaybeT . toMaybeT

instance MonadDist m => MonadBayes (RejectionT m) where
  condition b = unless b (fail "")
  factor = undefined
--  factor _ = error "RejectionT does not support soft conditioning"
