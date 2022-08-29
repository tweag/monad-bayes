{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- slower than Control.Monad.Bayes.Density.Free,
-- but much more elementary to understand. Just uses standard
-- monad transformer techniques.
-- @
module Control.Monad.Bayes.Density.State where

import Control.Monad.Bayes.Class (MonadSample (random))
import Control.Monad.State (MonadState (get, put), MonadTrans, StateT, evalStateT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT)
import Control.Monad.Writer (MonadWriter (..))

newtype Density m a = Density {runDensity :: WriterT [Double] (StateT [Double] m) a} deriving newtype (Functor, Applicative, Monad)

instance MonadTrans Density where
  lift = Density . lift . lift

instance Monad m => MonadState [Double] (Density m) where
  get = Density $ lift $ get
  put = Density . lift . put

instance Monad m => MonadWriter [Double] (Density m) where
  tell = Density . tell
  listen = Density . listen . runDensity
  pass = Density . pass . runDensity

instance MonadSample m => MonadSample (Density m) where
  random = do
    trace <- get
    x <- case trace of
      [] -> random
      r : xs -> put xs >> pure r
    tell [x]
    pure x

density :: Monad m => Density m b -> [Double] -> m (b, [Double])
density (Density m) = evalStateT (runWriterT m)
