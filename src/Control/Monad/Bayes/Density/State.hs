{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- slower than Control.Monad.Bayes.Density.Free,
-- but much more elementary to understand. Just uses standard
-- monad transformer techniques.
-- @
module Control.Monad.Bayes.Density.State where

import Control.Monad.Bayes.Class 
import Control.Monad.State (MonadState (get, put), MonadTrans, StateT, evalStateT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT)
import Control.Monad.Writer (MonadWriter, tell)
import Prelude hiding (Real)
import Numeric.AD

newtype Density m a = Density (WriterT [Real m] (StateT [Real m] m) a) deriving newtype (Functor, Applicative, Monad)

instance MonadTrans Density where
  lift = Density . lift . lift

instance (Real m ~ n, Monad m) => MonadState [n] (Density m) where
  get = Density $ lift $ get
  put = Density . lift . put

instance (Real m ~ n, Monad m) => MonadWriter [n] (Density m) where
  tell = Density . tell

instance (MonadSample m) => MonadSample (Density m) where
  type (Real (Density m)) = Real m
  random = do
    trace <- get
    x <- case trace of
      [] -> random
      r : xs -> put xs >> pure r
    tell [x]
    pure x

density :: Monad m => Density m b -> [Real m] -> m (b, [Real m])
density (Density m) = evalStateT (runWriterT m)

