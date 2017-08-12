{-|
Module      : Control.Monad.Bayes.Class
Description : Types for probabilistic modelling
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  GADTs
 #-}


module Control.Monad.Bayes.Class (
  MonadSample,
  random,
  uniform,
  normal,
  gamma,
  beta,
  bernoulli,
  categorical,
  geometric,
  poisson
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Writer
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.RWS hiding (tell)
-- import Control.Monad.Trans.List
-- import Control.Monad.Trans.Cont

import Statistics.Distribution
import Statistics.Distribution.Uniform (uniformDistr)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Gamma (gammaDistr)
import Statistics.Distribution.Beta (betaDistr)
import Statistics.Distribution.Geometric (geometric0)
import qualified Statistics.Distribution.Poisson as Poisson

import Data.Vector.Generic
import Control.Monad (when)

-- | Class of monads that can draw random variables.
class Monad m => MonadSample m where
  -- | A random variable distributed uniformly on [0,1].
  random :: m Double

  uniform :: Double -> Double -> m Double
  uniform a b = draw (uniformDistr a b)
  normal :: Double -> Double -> m Double
  normal m s = draw (normalDistr m s)
  gamma :: Double -> Double -> m Double
  gamma shape scale = draw (gammaDistr shape scale)
  beta :: Double -> Double -> m Double
  beta a b = draw (betaDistr a b)

  bernoulli :: Double -> m Bool
  bernoulli p = fmap (< p) random
  categorical :: Vector v Double => v Double -> m Int
  categorical ps = fromPMF (ps !)
  geometric :: Double -> m Int
  geometric p = discrete (geometric0 p)
  poisson :: Double -> m Int
  poisson lambda = discrete (Poisson.poisson lambda)

-- | Draw a value from a continuous distribution using iverse CDF.
draw :: (ContDistr d, MonadSample m) => d -> m Double
draw d = fmap (quantile d) random

-- | Draw a value from a discrete distribution using a sequence of draws from Bernoulli.
fromPMF :: MonadSample m => (Int -> Double) -> m Int
fromPMF p = f 0 1 where
  f i r = do
    when (r < 0) $ error "fromPMF: total PMF above 1"
    let q = p i
    when (q < 0 || q > 1) $ error "fromPMF: invalid probability value"
    b <- bernoulli (q / r)
    if b then pure i else f (i+1) (r-q)

-- | Draw a value from a discrete distributions using the PMF.
discrete :: (DiscreteDistr d, MonadSample m) => d -> m Int
discrete d = fromPMF (probability d)

-- | Monads that can score different execution paths.
class Monad m => MonadCond m where
  score :: Double -> m ()

-- | Monads that support both sampling and scoring.
class (MonadSample m, MonadCond m) => MonadInfer m

----------------------------------------------------------------------------
-- Instances that lift probabilistic effects to standard tranformers.

instance MonadSample m => MonadSample (IdentityT m) where
  random = lift random
  bernoulli = lift . bernoulli

instance MonadCond m => MonadCond (IdentityT m) where
  score = lift . score

instance MonadInfer m => MonadInfer (IdentityT m)


-- instance HasCustomReal m => HasCustomReal (MaybeT m) where
--   type CustomReal (MaybeT m) = CustomReal m
--
-- instance (Sampleable d m, Monad m) => Sampleable d (MaybeT m) where
--   sample = lift . sample
--
-- instance (Conditionable m, Monad m) => Conditionable (MaybeT m) where
--   factor = lift . factor
--
--
-- instance HasCustomReal m => HasCustomReal (ReaderT r m) where
--   type CustomReal (ReaderT r m) = CustomReal m
--
-- instance (Sampleable d m, Monad m) => Sampleable d (ReaderT r m) where
--   sample = lift . sample
--
-- instance (Conditionable m, Monad m) => Conditionable (ReaderT r m) where
--   factor = lift . factor
--
--
-- instance HasCustomReal m => HasCustomReal (WriterT w m) where
--   type CustomReal (WriterT w m) = CustomReal m
--
-- instance (Sampleable d m, Monad m, Monoid w) => Sampleable d (WriterT w m) where
--   sample = lift . sample
--
-- instance (Conditionable m, Monad m, Monoid w) => Conditionable (WriterT w m) where
--   factor = lift . factor
--
--
-- instance HasCustomReal m => HasCustomReal (StateT s m) where
--   type CustomReal (StateT s m) = CustomReal m
--
-- instance (Sampleable d m, Monad m) => Sampleable d (StateT s m) where
--   sample = lift . sample
--
-- instance (Conditionable m, Monad m) => Conditionable (StateT s m) where
--   factor = lift . factor
--
--
-- instance HasCustomReal m => HasCustomReal (RWST r w s m) where
--   type CustomReal (RWST r w s m) = CustomReal m
--
-- instance (Sampleable d m, Monad m, Monoid w) => Sampleable d (RWST r w s m) where
--   sample = lift . sample
--
-- instance (Conditionable m, Monad m, Monoid w) => Conditionable (RWST r w s m) where
--   factor = lift . factor
--
--
-- instance HasCustomReal m => HasCustomReal (ListT m) where
--   type CustomReal (ListT m) = CustomReal m
--
-- instance (Sampleable d m, Monad m) => Sampleable d (ListT m) where
--   sample = lift . sample
--
-- instance (Conditionable m, Monad m) => Conditionable (ListT m) where
--   factor = lift . factor
--
--
-- instance HasCustomReal m => HasCustomReal (ContT r m) where
--   type CustomReal (ContT r m) = CustomReal m
--
-- instance (Sampleable d m, Monad m) => Sampleable d (ContT r m) where
--   sample = lift . sample
--
-- instance (Conditionable m, Monad m) => Conditionable (ContT r m) where
--   factor = lift . factor
