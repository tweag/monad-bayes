{-|
Module      : Control.Monad.Bayes.Rejection
Description : Rejection-based probability monad
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Rejection (
  Rejection,
  runRejection,
  hoist
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Numeric.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple

-- | A probability monad that aborts when a condition is violated.
-- Only accepts factors in [0,1] range.
newtype Rejection m a = Rejection {toMaybeT :: (MaybeT m a)}
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | Run the probabilistic computation and produce a result if all conditions
-- are satisfied.
-- Throws an error if any factors exceed 1.
runRejection :: Rejection m a -> m (Maybe a)
runRejection = runMaybeT . toMaybeT

instance HasCustomReal m => HasCustomReal (Rejection m) where
  type CustomReal (Rejection m) = CustomReal m

instance (Sampleable d m, Monad m) => Sampleable d (Rejection m) where
  sample = lift . sample

instance (MonadDist m) => Conditionable (Rejection m) where
  factor w | w > 1 = error $ "Rejection: factor " ++ show (realToFrac w :: Double) ++ " is not in [0,1] range."
  factor w = do
    accept <- bernoulli (fromLogDomain w)
    unless accept (fail "")

instance MonadDist m => MonadDist (Rejection m)
instance MonadDist m => MonadBayes (Rejection m)

hoist :: (forall x. m x -> n x) -> Rejection m a -> Rejection n a
hoist f = Rejection . MaybeT . f . runRejection
