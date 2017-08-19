{-|
Module      : Control.Monad.Bayes.Parametric
Description : Models with global parameters
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Parametric (
  Parametric,
  parametric,
  getParam,
  withParam,
  hoist
) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans

import Control.Monad.Bayes.Class

-- | A transformer keeping track of the (hyper)parameters for the model.
newtype Parametric p m a = Parametric (ReaderT p m a)
  deriving(Functor, Applicative, Monad, MonadTrans, MonadIO, MonadSample, MonadCond, MonadInfer)

-- | Construct a computation with global parameters.
parametric :: (p -> m a) -> Parametric p m a
parametric = Parametric . ReaderT

-- | Get the value of parameters.
getParam :: Monad m => Parametric p m p
getParam = Parametric ask

-- | Run with specified parameters.
withParam :: Parametric p m a -> p -> m a
withParam (Parametric m) = runReaderT m

-- | Modify the transformed monad.
hoist :: (m a -> n a) -> Parametric p m a -> Parametric p n a
hoist f (Parametric m) = Parametric $ mapReaderT f m
