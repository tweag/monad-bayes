{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Bayes.Density.State where

import Control.Monad.Bayes.Class (MonadSample (random))
import Control.Monad.RWS
  ( MonadState (get, put),
    MonadTrans (..),
    MonadWriter (tell),
    RWST (RWST),
    evalRWST,
  )

newtype Density m a = Density (RWST () [Double] [Double] m a) deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance MonadSample m => MonadSample (Density m) where
  random = Density do
    trace <- get
    case trace of
      [] -> do
        r <- lift random
        tell [r]
        return r
      x : xs -> put xs >> tell [x] >> pure x

density :: Monad m => Density m b -> [Double] -> m (b, [Double])
density (Density m) = evalRWST m ()

example :: MonadSample m => m Double
example = do
  x <- random
  y <- random
  return (x + y)
