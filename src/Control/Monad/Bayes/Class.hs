{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE TypeFamilies #-}

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
-- model :: MonadInfer m => m Bool
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
  ( MonadSample(..),
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
    discrete,
    normalPdf,
    Bayesian (..),
    posterior,
    priorPredictive,
    posteriorPredictive,
    independent,
    mvNormal,
  )
where

import Control.Monad (replicateM, when)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Matrix hiding ((!))
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
import Prelude hiding (Real)
import Data.Kind (Type)

class (RealFloat (Real m), Monad m) => MonadSample m where
  type Real m :: Type
  -- random :: m (Real m)

-- | Monads that can draw random variables.
-- class Monad m => MonadSample m where
  -- | Draw from a uniform distribution.
  random ::
    -- | \(\sim \mathcal{U}(0, 1)\)
    m (Real m)

  -- | Draw from a uniform distribution.
  uniform ::
    -- | lower bound a
    (Real m) ->
    -- | upper bound b
    (Real m) ->
    -- | \(\sim \mathcal{U}(a, b)\).
    m (Real m)
  -- uniform a b = draw (uniformDistr a b)

  -- | Draw from a normal distribution.
  normal ::
    -- | mean μ
    (Real m) ->
    -- | standard deviation σ
    (Real m) ->
    -- | \(\sim \mathcal{N}(\mu, \sigma^2)\)
    m (Real m)
  -- normal m s = draw (normalDistr m s)

  -- | Draw from a gamma distribution.
  gamma ::
    -- | shape k
    (Real m) ->
    -- | scale θ
    (Real m) ->
    -- | \(\sim \Gamma(k, \theta)\)
    m (Real m)
  -- gamma shape scale = draw (gammaDistr shape scale)

  -- | Draw from a beta distribution.
  beta ::
    -- | shape α
    (Real m) ->
    -- | shape β
    (Real m) ->
    -- | \(\sim \mathrm{Beta}(\alpha, \beta)\)
    m (Real m)
  -- beta a b = draw (betaDistr a b)

  -- | Draw from a Bernoulli distribution.
  bernoulli ::
    -- | probability p
    Real m ->
    -- | \(\sim \mathrm{B}(1, p)\)
    m Bool
  bernoulli p = fmap (< p) random

  -- | Draw from a categorical distribution.
  categorical ::
    Vector v (Real m) =>
    -- | event probabilities
    v (Real m) ->
    -- | outcome category
    m Int
  -- categorical ps = if VG.null ps then error "empty input list" else fromPMF (ps !)

  -- | Draw from a categorical distribution in the log domain.
  logCategorical ::
    (Vector v (Log (Real m)), Vector v (Real m)) =>
    -- | event probabilities
    v (Log (Real m)) ->
    -- | outcome category
    m Int
  logCategorical = categorical . VG.map (exp . ln)

  -- | Draw from a discrete uniform distribution.
  uniformD ::
    -- | observable outcomes @xs@
    [a] ->
    -- | \(\sim \mathcal{U}\{\mathrm{xs}\}\)
    m a
  uniformD xs = do
    let n = Prelude.length xs
    i <- categorical $ V.replicate n (1 / fromIntegral n)
    return (xs !! i)

  -- | Draw from a geometric distribution.
  geometric ::
    -- | success rate p
    Double ->
    -- | \(\sim\) number of failed Bernoulli trials with success probability p before first success
    m Int
  -- geometric = discrete . geometric0

  -- | Draw from a Poisson distribution.
  poisson ::
    -- | parameter λ
    Double ->
    -- | \(\sim \mathrm{Pois}(\lambda)\)
    m Int
  -- poisson = discrete . Poisson.poisson

  -- | Draw from a Dirichlet distribution.
  dirichlet ::
    Vector v (Real m) =>
    -- | concentration parameters @as@
    v (Real m) ->
    -- | \(\sim \mathrm{Dir}(\mathrm{as})\)
    m (v (Real m))
  dirichlet as = do
    xs <- VG.mapM (`gamma` 1) as
    let s = VG.sum xs
    let ys = VG.map (/ s) xs
    return ys

-- | Draw from a continuous distribution using the inverse cumulative density
-- function.
draw :: (ContDistr d, MonadSample m, Real m ~ Double) => d -> m Double
draw d = fmap (quantile d) random

-- | Draw from a discrete distribution using a sequence of draws from
-- Bernoulli.
fromPMF :: (MonadSample m, Real m ~ Double) => (Int -> Double) -> m Int
fromPMF p = f 0 1
  where
    f i r = do
      when (r < 0) $ error "fromPMF: total PMF above 1"
      let q = p i
      when (q < 0 || q > 1) $ error "fromPMF: invalid probability value"
      b <- bernoulli (q / r)
      if b then pure i else f (i + 1) (r - q)

-- | Draw from a discrete distributions using the probability mass function.
discrete :: (DiscreteDistr d, MonadSample m, Real m ~ Double) => d -> m Int
discrete = fromPMF . probability

-- | Monads that can score different execution paths.
class Monad m => MonadCond m where
  -- | Record a likelihood.
  score ::
    -- | likelihood of the execution path
    Log (Real m) ->
    m ()

-- | Synonym for 'score'.
factor ::
  MonadCond m =>
  -- | likelihood of the execution path
  Log (Real m) ->
  m ()
factor = score

-- | Hard conditioning.
condition :: (MonadCond m, RealFloat (Real m)) => Bool -> m ()
condition b = score $ if b then 1 else 0

independent :: Applicative m => Int -> m a -> m [a]
independent = replicateM

-- | Monads that support both sampling and scoring.
class (MonadSample m, MonadCond m) => MonadInfer m

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

--------------------

-- | multivariate normal
mvNormal :: MonadSample m => V.Vector (Real m) -> Matrix (Real m) -> m (V.Vector (Real m))
mvNormal mu bigSigma = do
  let n = length mu
  ss <- replicateM n (normal 0 1)
  let bigL = cholDecomp bigSigma
  let ts = (colVector mu) + bigL `multStd` (colVector $ V.fromList ss)
  return $ getCol 1 ts

-- | a useful datatype for expressing bayesian models
data Bayesian m z o = Bayesian
  { latent :: m z, -- prior over latent variable Z
    generative :: z -> m o, -- distribution over observations given Z=z
    likelihood :: z -> o -> Log (Real m) -- p(o|z)
  }

posterior :: (MonadInfer m, Foldable f, Functor f) => Bayesian m z o -> f o -> m z
posterior Bayesian {..} os = do
  z <- latent
  factor $ product $ fmap (likelihood z) os
  return z

priorPredictive :: Monad m => Bayesian m a b -> m b
priorPredictive bm = latent bm >>= generative bm

posteriorPredictive ::
  (MonadInfer m, Foldable f, Functor f) =>
  Bayesian m a b ->
  f b ->
  m b
posteriorPredictive bm os = posterior bm os >>= generative bm

----------------------------------------------------------------------------
-- Instances that lift probabilistic effects to standard tranformers.

instance MonadSample m => MonadSample (IdentityT m) where
  type Real (IdentityT m) = Real m
  random = lift random
  bernoulli = lift . bernoulli

instance MonadCond m => MonadCond (IdentityT m) where
  score = lift . score

instance MonadInfer m => MonadInfer (IdentityT m)

instance MonadSample m => MonadSample (ExceptT e m) where
  type Real (ExceptT e m) = Real m
  random = lift random
  uniformD = lift . uniformD

instance MonadCond m => MonadCond (ExceptT e m) where
  score = lift . score

instance MonadInfer m => MonadInfer (ExceptT e m)

instance MonadSample m => MonadSample (ReaderT r m) where
  type Real (ReaderT r m) = Real m
  random = lift random
  bernoulli = lift . bernoulli

instance MonadCond m => MonadCond (ReaderT r m) where
  score = lift . score

instance MonadInfer m => MonadInfer (ReaderT r m)

instance (Monoid w, MonadSample m) => MonadSample (WriterT w m) where
  type Real (WriterT w m) = Real m
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical

instance (Monoid w, MonadCond m) => MonadCond (WriterT w m) where
  score = lift . score

instance (Monoid w, MonadInfer m) => MonadInfer (WriterT w m)

instance MonadSample m => MonadSample (StateT s m) where
  type Real (StateT s m) = Real m
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical
  uniformD = lift . uniformD

instance MonadCond m => MonadCond (StateT s m) where
  score = lift . score

instance MonadInfer m => MonadInfer (StateT s m)

instance MonadSample m => MonadSample (ListT m) where
  type Real (ListT m) = Real m
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical

instance MonadCond m => MonadCond (ListT m) where
  score = lift . score

instance MonadInfer m => MonadInfer (ListT m)

instance MonadSample m => MonadSample (ContT r m) where
  type Real (ContT r m) = Real m
  random = lift random

instance MonadCond m => MonadCond (ContT r m) where
  score = lift . score

instance MonadInfer m => MonadInfer (ContT r m)
