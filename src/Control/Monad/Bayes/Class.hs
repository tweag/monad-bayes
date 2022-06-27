{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Control.Monad.Bayes.Class
-- Description : Types for probabilistic modelling
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- This module defines 'MonadInfer', which can be used to represent a simple model
-- like the following:
--
-- @
-- import Control.Monad (when)
-- import Control.Monad.Bayes.Class
--
-- model :: MonadInfer n m => m Bool
-- model = do
--   rain <- bernoulli 0.3
--   sprinkler <-
--     bernoulli $
--     if rain
--       then 0.1
--       else 0.4
--   let wetProb =
--     case (rain, sprinkler) of
--       (True,  True)  -> 0.98
--       (True,  False) -> 0.80
--       (False, True)  -> 0.90
--       (False, False) -> 0.00
--   score wetProb
--   return rain
-- @
module Control.Monad.Bayes.Class
  ( MonadSample (randomGeneric),
    random,
    uniform,
    normal,
    gamma,
    beta,
    bernoulli,
    categorical,
    logCategorical,
    geometric,
    poisson,
    dirichlet,
    MonadCond (scoreGeneric),
    score,
    factor,
    condition,
    MonadInfer,
    discrete,
    normalPdf,
    -- Bayesian(Bayesian),
    -- posterior,
    uniformD,
    IdentityN (..),
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Vector qualified as V
import Data.Vector.Generic as VG (Vector, map, mapM, null, sum, (!))
import Numeric.Log (Log (..))
import Statistics.Distribution
  ( ContDistr (logDensity, quantile),
    DiscreteDistr (probability),
  )
import Statistics.Distribution.Beta (betaDistr)
import Statistics.Distribution.Gamma (gammaDistr)
import Statistics.Distribution.Geometric (geometric0)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Poisson qualified as Poisson
import Statistics.Distribution.Uniform (uniformDistr)

random :: MonadSample Double m => m Double Double
random = randomGeneric

-- | Monads that can draw random variables.
class (RealFloat n, Monad (m n)) => MonadSample n m where
  -- | Draw from a uniform distribution.
  randomGeneric ::
    -- | \(\sim \mathcal{U}(0, 1)\)
    m n n

  -- | Draw from a uniform distribution.
  uniform ::
    -- | lower bound a
    n ->
    -- | upper bound b
    n ->
    -- | \(\sim \mathcal{U}(a, b)\).
    m n n
  uniform a b = undefined -- draw (uniformDistr a b)

  -- | Draw from a normal distribution.
  normal ::
    -- | mean μ
    n ->
    -- | standard deviation σ
    n ->
    -- | \(\sim \mathcal{N}(\mu, \sigma^2)\)
    m n n
  normal m s = undefined -- draw (normalDistr m s)

  -- | Draw from a gamma distribution.
  gamma ::
    -- | shape k
    n ->
    -- | scale θ
    n ->
    -- | \(\sim \Gamma(k, \theta)\)
    m n n
  gamma shape scale = undefined --draw (gammaDistr shape scale)

  -- | Draw from a beta distribution.
  beta ::
    -- | shape α
    n ->
    -- | shape β
    n ->
    -- | \(\sim \mathrm{Beta}(\alpha, \beta)\)
    m n n
  beta a b = undefined -- draw (betaDistr a b)

  -- | Draw from a Bernoulli distribution.
  bernoulli ::
    -- | probability p
    n ->
    -- | \(\sim \mathrm{B}(1, p)\)
    m n Bool
  bernoulli p = undefined

  -- if (-0.01) <= p && p <= 1.01 -- leave a little room for floating point errors
  --   then fmap (< p) random
  --   else error $ "bernoulli parameter p must be in range [0,1], but is: " <> show p

  -- if (-0.01) <= p && p <= 1.01 -- leave a little room for floating point errors
  --   then fmap (< p) random
  --   else error $ "bernoulli parameter p must be in range [0,1], but is: " <> show p

  -- | Draw from a categorical distribution.
  categorical ::
    Vector v n =>
    -- | event probabilities
    v n ->
    -- | outcome category
    m n Int
  categorical ps = undefined -- if VG.null ps then error "empty input list" else fromPMF (ps !)

  -- | Draw from a categorical distribution in the log domain.
  logCategorical ::
    (Vector v (Log n), Vector v n) =>
    -- | event probabilities
    v (Log n) ->
    -- | outcome category
    m n Int
  logCategorical = undefined -- categorical . VG.map (exp . ln)

  -- | Draw from a geometric distribution.
  geometric ::
    -- | success rate p
    n ->
    -- | \(\sim\) number of failed Bernoulli trials with success probability p before first success
    m n Int
  geometric = undefined -- discrete . geometric0

  -- | Draw from a Poisson distribution.
  poisson ::
    -- | parameter λ
    n ->
    -- | \(\sim \mathrm{Pois}(\lambda)\)
    m n Int
  poisson = undefined -- discrete . Poisson.poisson

  -- | Draw from a Dirichlet distribution.
  dirichlet ::
    Vector v n =>
    -- | concentration parameters @as@
    v n ->
    -- | \(\sim \mathrm{Dir}(\mathrm{as})\)
    m n (v n)
  dirichlet as = undefined

-- do
--   xs <- VG.mapM (`gamma` 1) as
--   let s = VG.sum xs
--   let ys = VG.map (/ s) xs
--   return ys

-- | Draw from a continuous distribution using the inverse cumulative density
-- function.
draw :: (ContDistr d, MonadSample Double m) => d -> m Double Double
draw d = fmap (quantile d) random

-- | Draw from a discrete distribution using a sequence of draws from
-- Bernoulli.
fromPMF :: MonadSample Double m => (Int -> Double) -> m Double Int
fromPMF p = f 0 1
  where
    f i r = do
      when (r < 0) $ error "fromPMF: total PMF above 1"
      let q = p i
      when (q < 0 || q > 1) $ error "fromPMF: invalid probability value"
      b <- bernoulli (q / r)
      if b then pure i else f (i + 1) (r - q)

-- | Draw from a discrete distributions using the probability mass function.
discrete :: (DiscreteDistr d, MonadSample Double m) => d -> m Double Int
discrete = fromPMF . probability

-- | Monads that can score different execution paths.
class (RealFloat n, Monad (m n)) => MonadCond n m where
  -- | Record a likelihood.
  scoreGeneric ::
    -- | likelihood of the execution path
    Log n ->
    m n ()

score :: MonadCond Double m => Log Double -> m Double ()
score = scoreGeneric

-- | Synonym for 'score'.
factor :: MonadCond Double m => Log Double -> m Double ()
factor = score

-- | Hard conditioning.
condition :: (RealFloat n, MonadCond n m) => Bool -> m n ()
condition b = scoreGeneric $ if b then 1 else 0

-- | Monads that support both sampling and scoring.
class (MonadSample n m, MonadCond n m) => MonadInfer n m

-- | Probability density function of the normal distribution.
normalPdf ::
  -- | mean μ
  Double ->
  -- | standard deviation σ
  Double ->
  -- | sample x
  Double ->
  -- | relative likelihood of observing sample x in \(\mathcal{N}(\mu, \sigma^2)\)
  Log Double
normalPdf mu sigma x = Exp $ logDensity (normalDistr mu sigma) x

-- | Draw from a discrete uniform distribution.
uniformD ::
  MonadSample Double m =>
  -- | observable outcomes @xs@
  [a] ->
  -- | \(\sim \mathcal{U}\{\mathrm{xs}\}\)
  m Double a
uniformD xs = do
  let n = Prelude.length xs
  i <- categorical $ V.replicate n (1 / fromIntegral n)
  return (xs !! i)

--------------------

-- -- | a useful datatype for expressing bayesian models
-- data Bayesian m z o = Bayesian
--   { latent :: m z, -- prior over latent variable Z
--     generative :: z -> m o, -- distribution over observations given Z=z
--     likelihood :: z -> o -> Log Double -- p(o|z)
--   }

-- posterior :: (MonadInfer Double m, Foldable f, Functor f) => Bayesian m z o -> f o -> m z
-- posterior Bayesian {..} os = do
--   z <- latent
--   factor $ product $ fmap (likelihood z) os
--   return z

----------------------------------------------------------------------------
-- Instances that lift probabilistic effects to standard tranformers.

-- instance MonadSample n m => MonadSample n (IdentityT m) where
--   randomGeneric = lift randomGeneric
--   bernoulli = lift . bernoulli

-- instance MonadCond n m => MonadCond n (IdentityT m) where
--   scoreGeneric = lift . scoreGeneric

-- instance MonadInfer n m => MonadInfer n (IdentityT m)

-- instance MonadSample n m => MonadSample n (ExceptT e m) where
--   randomGeneric = lift randomGeneric
--   categorical = lift . categorical

-- instance MonadCond n m => MonadCond n (ExceptT e m) where
--   scoreGeneric = lift . scoreGeneric

-- instance MonadInfer n m => MonadInfer n (ExceptT e m)

-- instance MonadSample n m => MonadSample n (ReaderT r m) where
--   randomGeneric = lift randomGeneric
--   bernoulli = lift . bernoulli

-- instance MonadCond n m => MonadCond n (ReaderT r m) where
--   scoreGeneric = lift . scoreGeneric

-- instance MonadInfer n m => MonadInfer n (ReaderT r m)

-- instance (Monoid w, MonadSample n m) => MonadSample n (WriterT w m) where
--   randomGeneric = lift randomGeneric
--   bernoulli = lift . bernoulli
--   categorical = lift . categorical

-- instance (Monoid w, MonadCond n m) => MonadCond n (WriterT w m) where
--   scoreGeneric = lift . scoreGeneric

-- instance (Monoid w, MonadInfer n m) => MonadInfer n (WriterT w m)

-- instance MonadSample n m => MonadSample n (StateT s m) where
--   randomGeneric = lift randomGeneric
--   bernoulli = lift . bernoulli
--   categorical = lift . categorical

-- instance MonadCond n m => MonadCond n (StateT s m) where
--   scoreGeneric = lift . scoreGeneric

-- instance MonadInfer n m => MonadInfer n (StateT s m)

-- instance MonadSample n m => MonadSample n (ListT m) where
--   randomGeneric = lift randomGeneric
--   bernoulli = lift . bernoulli
--   categorical = lift . categorical

-- instance MonadCond n m => MonadCond n (ListT m) where
--   scoreGeneric = lift . scoreGeneric

-- instance MonadInfer n m => MonadInfer n (ListT m)

-- instance MonadSample n m => MonadSample n (ContT r m) where
--   randomGeneric = lift randomGeneric

-- instance MonadCond n m => MonadCond n (ContT r m) where
--   scoreGeneric = lift . scoreGeneric

-- instance MonadInfer n m => MonadInfer n (ContT r m)
data IdentityN n a = IdentityN {runIdentityN :: a} deriving (Functor)

instance Applicative (IdentityN n)

instance Monad (IdentityN n) where
  return a = IdentityN a
  a >>= f = (f $ runIdentityN a)
