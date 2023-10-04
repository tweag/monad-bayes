{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Slower than Control.Monad.Bayes.Density.Free, so not used by default,
-- but more elementary to understand. Just uses standard
-- monad transformer techniques.
module Control.Monad.Bayes.Density.State where

import Control.Monad.Bayes.Class (MonadDistribution (random))
import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
import Control.Monad.Writer

newtype DensityT m a = DensityT {getDensityT :: WriterT [Double] (StateT [Double] m) a} deriving newtype (Functor, Applicative, Monad)

instance MonadTrans DensityT where
  lift = DensityT . lift . lift

instance (Monad m) => MonadState [Double] (DensityT m) where
  get = DensityT $ lift $ get
  put = DensityT . lift . put

instance (Monad m) => MonadWriter [Double] (DensityT m) where
  tell = DensityT . tell
  listen = DensityT . listen . getDensityT
  pass = DensityT . pass . getDensityT

instance (MonadDistribution m) => MonadDistribution (DensityT m) where
  random = do
    trace <- get
    x <- case trace of
      [] -> random
      r : xs -> put xs >> pure r
    tell [x]
    pure x

runDensityT :: (Monad m) => DensityT m b -> [Double] -> m (b, [Double])
runDensityT (DensityT m) = evalStateT (runWriterT m)
