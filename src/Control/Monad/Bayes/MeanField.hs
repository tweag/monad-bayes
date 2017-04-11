{-|
Module      : Control.Monad.Bayes.MeanField
Description : Mean field variational approximation
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

-- It is safe to enable UndecidableInstances here so long as we don't write any Sampleable instances
-- where m doesn't shrink.
{-# LANGUAGE
  UndecidableInstances
 #-}

module Control.Monad.Bayes.MeanField (
  MeanFieldNormal,
  meanFieldNormal
) where

import Prelude hiding (map, unzip)

import Data.Vector
import Numeric.LinearAlgebra hiding (Vector, (!))
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader

import Statistics.Distribution.Polymorphic.MVNormal

import Control.Monad.Bayes.Simple hiding (Parametric)
import Control.Monad.Bayes.Parametric
import Control.Monad.Bayes.Inference.Proposal

newtype MeanFieldNormal m a = MeanFieldNormal (StateT Int (ReaderT (Vector (CustomReal m, CustomReal m)) m) a)
  deriving(Functor,Applicative,Monad,MonadIO)

instance MonadTrans MeanFieldNormal where
  lift = MeanFieldNormal . lift . lift

instance HasCustomReal m => HasCustomReal (MeanFieldNormal m) where
  type CustomReal (MeanFieldNormal m)= CustomReal m

instance {-# OVERLAPPING #-} (Conditionable m, Sampleable (Normal (CustomReal m)) m, Domain d ~ CustomReal m, RealNum d ~ CustomReal m, Monad m, Density d) => Sampleable d (MeanFieldNormal m) where
  sample d = MeanFieldNormal $ do
    params <- ask
    index <- get
    let (m,s) = params ! index
    put (index + 1)
    d `proposingFrom` normalDist m s

instance {-# OVERLAPPING #-} (Sampleable MVNormal m, Conditionable m, CustomReal m ~ Double, Monad m) => Sampleable MVNormal (MeanFieldNormal m) where
  sample d = MeanFieldNormal $ do
    let n = dim d
    params <- ask
    index <- get
    let (ms, ss) = unzip $ slice index n params
    put (index + n)
    d `proposingFrom` mvnormalDist (convert ms) (trustSym $ diag $ convert $ map (^ (2 :: Int)) ss)

instance {-# OVERLAPPING #-} (Sampleable (Discrete r k) m, Monad m) => Sampleable (Discrete r k) (MeanFieldNormal m) where
  sample = lift . sample

instance (Conditionable m, Monad m) => Conditionable (MeanFieldNormal m) where
  factor = lift . factor

instance MonadBayes m => MonadDist (MeanFieldNormal m)
instance MonadBayes m => MonadBayes (MeanFieldNormal m)

meanFieldNormal :: Monad m => MeanFieldNormal m a -> Parametric (Vector (CustomReal m, CustomReal m)) m a
meanFieldNormal (MeanFieldNormal m) = parametric $ runReaderT $ evalStateT m 0
