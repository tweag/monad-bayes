{-|
Module      : Control.Monad.Bayes.Class
Description : Types for probabilistic modelling
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Class (
  MonadSample,
  random,
  uniform,
  normal,
  gamma,
  beta,
  bernoulli,
  categorical,
  logCategorical,
  uniformD,
  geometric,
  poisson,
  dirichlet,
  MonadCond,
  score,
  factor,
  condition,
  MonadInfer,
  normalPdf
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS hiding (tell)
import Control.Monad.Trans.List
import Control.Monad.Trans.Cont

import Statistics.Distribution
import Statistics.Distribution.Uniform (uniformDistr)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Gamma (gammaDistr)
import Statistics.Distribution.Beta (betaDistr)
import Statistics.Distribution.Geometric (geometric0)
import qualified Statistics.Distribution.Poisson as Poisson

import Numeric.Log

import Data.Vector.Generic as VG
import qualified Data.Vector as V
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
  uniformD :: [a] -> m a
  uniformD xs = do
    let n = Prelude.length xs
    i <- categorical $ V.replicate n (1 / fromIntegral n)
    return (xs !! i)
  logCategorical :: (Vector v (Log Double), Vector v Double) => v (Log Double) -> m Int
  logCategorical = categorical . VG.map (exp . ln)
  geometric :: Double -> m Int
  geometric p = discrete (geometric0 p)
  poisson :: Double -> m Int
  poisson lambda = discrete (Poisson.poisson lambda)

  dirichlet :: Vector v Double => v Double -> m (v Double)
  dirichlet as = do
    xs <- VG.mapM (`gamma` 1) as
    let s = VG.sum xs
    let ys = VG.map (/ s) xs
    return ys

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
  score :: Log Double -> m ()

-- | Synonym for 'score'.
factor :: MonadCond m => Log Double -> m ()
factor = score

-- | Hard conditioning.
condition :: MonadCond m => Bool -> m ()
condition b = score $ if b then 1 else 0

-- | Monads that support both sampling and scoring.
class (MonadSample m, MonadCond m) => MonadInfer m

normalPdf :: Double -> Double -> Double -> Log Double
normalPdf mu sigma x = Exp $ logDensity (normalDistr mu sigma) x

----------------------------------------------------------------------------
-- Instances that lift probabilistic effects to standard tranformers.

instance MonadSample m => MonadSample (IdentityT m) where
  random = lift random
  bernoulli = lift . bernoulli

instance MonadCond m => MonadCond (IdentityT m) where
  score = lift . score

instance MonadInfer m => MonadInfer (IdentityT m)


instance MonadSample m => MonadSample (MaybeT m) where
  random = lift random

instance MonadCond m => MonadCond (MaybeT m) where
  score = lift . score

instance MonadInfer m => MonadInfer (MaybeT m)


instance MonadSample m => MonadSample (ReaderT r m) where
  random = lift random
  bernoulli = lift . bernoulli

instance MonadCond m => MonadCond (ReaderT r m) where
  score = lift . score

instance MonadInfer m => MonadInfer (ReaderT r m)


instance (Monoid w, MonadSample m) => MonadSample (WriterT w m) where
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical

instance (Monoid w, MonadCond m) => MonadCond (WriterT w m) where
  score = lift . score

instance (Monoid w, MonadInfer m) => MonadInfer (WriterT w m)


instance MonadSample m => MonadSample (StateT s m) where
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical

instance MonadCond m => MonadCond (StateT s m) where
  score = lift . score

instance MonadInfer m => MonadInfer (StateT s m)


instance (MonadSample m, Monoid w) => MonadSample (RWST r w s m) where
  random = lift random

instance (MonadCond m, Monoid w) => MonadCond (RWST r w s m) where
  score = lift . score

instance (MonadInfer m, Monoid w) => MonadInfer (RWST r w s m)


instance MonadSample m => MonadSample (ListT m) where
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical

instance MonadCond m => MonadCond (ListT m) where
  score = lift . score

instance MonadInfer m => MonadInfer (ListT m)


instance MonadSample m => MonadSample (ContT r m) where
  random = lift random

instance MonadCond m => MonadCond (ContT r m) where
  score = lift . score

instance MonadInfer m => MonadInfer (ContT r m)
