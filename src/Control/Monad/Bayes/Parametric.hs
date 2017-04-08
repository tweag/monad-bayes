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
  withParam
) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans

import Control.Monad.Bayes.Class hiding (Parametric)
import Control.Monad.Bayes.Simple hiding (Parametric)

newtype Parametric p m a = Parametric (ReaderT p m a)
  deriving(Functor, Applicative, Monad, MonadTrans, MonadIO)

instance HasCustomReal m => HasCustomReal (Parametric p m) where
  type CustomReal (Parametric p m) = CustomReal m

instance Sampleable d m => Sampleable d (Parametric p m) where
  sample = parametric . const . sample

instance Conditionable m => Conditionable (Parametric p m) where
  factor = parametric . const . factor

instance MonadDist m => MonadDist (Parametric p m)
instance MonadBayes m => MonadBayes (Parametric p m)

parametric :: (p -> m a) -> Parametric p m a
parametric = Parametric . ReaderT

getParam :: Monad m => Parametric p m p
getParam = Parametric ask

withParam :: Parametric p m a -> p -> m a
withParam (Parametric m) = runReaderT m
