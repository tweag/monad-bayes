{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
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
-- This module defines 'MonadMeasure', which can be used to represent any probabilistic program,
-- such as the following:
--
-- @
-- import Control.Monad (when)
-- import Control.Monad.Bayes.Class
--
-- model :: MonadMeasure m => m Bool
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
  ( MonadDistribution,
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
    MonadFactor,
    score,
    factor,
    condition,
    MonadMeasure,
    discrete,
    normalPdf,
    Bayesian (..),
    poissonPdf,
    posterior,
    priorPredictive,
    posteriorPredictive,
    independent,
    mvNormal,
    Histogram,
    histogram,
    histogramToList,
    Distribution,
    Measure,
    Kernel,
    Log (ln, Exp),
    MonadMeasureTrans (..),
  )
where

import Control.Arrow (Arrow (second))
import Control.Monad (replicateM, when)
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, lift)
import Control.Monad.Identity (IdentityT)
import Control.Monad.List (ListT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer (WriterT)
import Data.Histogram qualified as H
import Data.Histogram.Fill qualified as H
import Data.Kind (Type)
import Data.Matrix
  ( Matrix,
    cholDecomp,
    colVector,
    getCol,
    multStd,
  )
import Data.Vector qualified as V
import Data.Vector.Generic as VG (Vector, map, mapM, null, sum, (!))
import Numeric.Log (Log (..))
import Statistics.Distribution
  ( ContDistr (logDensity, quantile),
    DiscreteDistr (logProbability, probability),
  )
import Statistics.Distribution.Beta (betaDistr)
import Statistics.Distribution.Gamma (gammaDistr)
import Statistics.Distribution.Geometric (geometric0)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Poisson qualified as Poisson
import Statistics.Distribution.Uniform (uniformDistr)

-- | Monads that can draw random variables.
class Monad m => MonadDistribution m where
  -- | Draw from a uniform distribution.
  random ::
    -- | \(\sim \mathcal{U}(0, 1)\)
    m Double

  -- | Draw from a uniform distribution.
  uniform ::
    -- | lower bound a
    Double ->
    -- | upper bound b
    Double ->
    -- | \(\sim \mathcal{U}(a, b)\).
    m Double
  uniform a b = draw (uniformDistr a b)

  -- | Draw from a normal distribution.
  normal ::
    -- | mean μ
    Double ->
    -- | standard deviation σ
    Double ->
    -- | \(\sim \mathcal{N}(\mu, \sigma^2)\)
    m Double
  normal m s = draw (normalDistr m s)

  -- | Draw from a gamma distribution.
  gamma ::
    -- | shape k
    Double ->
    -- | scale θ
    Double ->
    -- | \(\sim \Gamma(k, \theta)\)
    m Double
  gamma shape scale = draw (gammaDistr shape scale)

  -- | Draw from a beta distribution.
  beta ::
    -- | shape α
    Double ->
    -- | shape β
    Double ->
    -- | \(\sim \mathrm{Beta}(\alpha, \beta)\)
    m Double
  beta a b = draw (betaDistr a b)

  -- | Draw from a Bernoulli distribution.
  bernoulli ::
    -- | probability p
    Double ->
    -- | \(\sim \mathrm{B}(1, p)\)
    m Bool
  bernoulli p = fmap (< p) random

  -- | Draw from a categorical distribution.
  categorical ::
    Vector v Double =>
    -- | event probabilities
    v Double ->
    -- | outcome category
    m Int
  categorical ps = if VG.null ps then error "empty input list" else fromPMF (ps !)

  -- | Draw from a categorical distribution in the log domain.
  logCategorical ::
    (Vector v (Log Double), Vector v Double) =>
    -- | event probabilities
    v (Log Double) ->
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
  geometric = discrete . geometric0

  -- | Draw from a Poisson distribution.
  poisson ::
    -- | parameter λ
    Double ->
    -- | \(\sim \mathrm{Pois}(\lambda)\)
    m Int
  poisson = discrete . Poisson.poisson

  -- | Draw from a Dirichlet distribution.
  dirichlet ::
    Vector v Double =>
    -- | concentration parameters @as@
    v Double ->
    -- | \(\sim \mathrm{Dir}(\mathrm{as})\)
    m (v Double)
  dirichlet as = do
    xs <- VG.mapM (`gamma` 1) as
    let s = VG.sum xs
    let ys = VG.map (/ s) xs
    return ys

-- | Draw from a continuous distribution using the inverse cumulative density
-- function.
draw :: (ContDistr d, MonadDistribution m) => d -> m Double
draw d = fmap (quantile d) random

-- | Draw from a discrete distribution using a sequence of draws from
-- Bernoulli.
fromPMF :: MonadDistribution m => (Int -> Double) -> m Int
fromPMF p = f 0 1
  where
    f i r = do
      when (r < 0) $ error "fromPMF: total PMF above 1"
      let q = p i
      when (q < 0 || q > 1) $ error "fromPMF: invalid probability value"
      b <- bernoulli (q / r)
      if b then pure i else f (i + 1) (r - q)

-- | Draw from a discrete distributions using the probability mass function.
discrete :: (DiscreteDistr d, MonadDistribution m) => d -> m Int
discrete = fromPMF . probability

-- | Monads that can score different execution paths.
class Monad m => MonadFactor m where
  -- | Record a likelihood.
  score ::
    -- | likelihood of the execution path
    Log Double ->
    m ()

-- | Synonym for 'score'.
factor ::
  MonadFactor m =>
  -- | likelihood of the execution path
  Log Double ->
  m ()
factor = score

-- | synonym for pretty type signatures, but note that (A -> Distribution B) won't work as intended: for that, use Kernel
-- Also note that the use of RankNTypes means performance may take a hit: really the main point of these signatures is didactic
type Distribution a = forall m. MonadDistribution m => m a

type Measure a = forall m. MonadMeasure m => m a

type Kernel a b = forall m. MonadMeasure m => a -> m b

-- | Hard conditioning.
condition :: MonadFactor m => Bool -> m ()
condition b = score $ if b then 1 else 0

independent :: Applicative m => Int -> m a -> m [a]
independent = replicateM

-- | Monads that support both sampling and scoring.
class (MonadDistribution m, MonadFactor m) => MonadMeasure m

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

poissonPdf :: Double -> Integer -> Log Double
poissonPdf rate n = Exp $ logProbability (Poisson.poisson rate) (fromIntegral n)

-- | multivariate normal
mvNormal :: MonadDistribution m => V.Vector Double -> Matrix Double -> m (V.Vector Double)
mvNormal mu bigSigma = do
  let n = length mu
  ss <- replicateM n (normal 0 1)
  let bigL = cholDecomp bigSigma
  let ts = (colVector mu) + bigL `multStd` (colVector $ V.fromList ss)
  return $ getCol 1 ts

-- | a useful datatype for expressing bayesian models
data Bayesian m z o = Bayesian
  { prior :: m z, -- prior over latent variable Z; p(z)
    generative :: z -> m o, -- distribution over observations given Z=z; p(o|Z=z)
    likelihood :: z -> o -> Log Double -- p(o|z)
  }

-- | p(z|o)
posterior :: (MonadMeasure m, Foldable f, Functor f) => Bayesian m z o -> f o -> m z
posterior Bayesian {..} os = do
  z <- prior
  factor $ product $ fmap (likelihood z) os
  return z

priorPredictive :: Monad m => Bayesian m a b -> m b
priorPredictive bm = prior bm >>= generative bm

posteriorPredictive ::
  (MonadMeasure m, Foldable f, Functor f) =>
  Bayesian m a b ->
  f b ->
  m b
posteriorPredictive bm os = posterior bm os >>= generative bm

-- helper funcs
--------------------

type Histogram = H.Histogram H.BinD Double

histogram :: Int -> [(Double, Log Double)] -> Histogram
histogram n v = H.fillBuilder buildr $ fmap (second (ln . exp)) v
  where
    v1 = fmap fst v
    mi = Prelude.minimum v1
    ma = Prelude.maximum v1
    bins = H.binD mi n ma
    buildr = H.mkWeighted bins

histogramToList :: Histogram -> [(Double, Double)]
histogramToList = H.asList

----------------------------------------------------------------------------
-- Instances that lift probabilistic effects to standard tranformers.

instance MonadDistribution m => MonadDistribution (IdentityT m) where
  random = lift random
  bernoulli = lift . bernoulli

instance MonadFactor m => MonadFactor (IdentityT m) where
  score = lift . score

instance MonadMeasure m => MonadMeasure (IdentityT m)

instance MonadDistribution m => MonadDistribution (ExceptT e m) where
  random = lift random
  uniformD = lift . uniformD

instance MonadFactor m => MonadFactor (ExceptT e m) where
  score = lift . score

instance MonadMeasure m => MonadMeasure (ExceptT e m)

instance MonadDistribution m => MonadDistribution (ReaderT r m) where
  random = lift random
  bernoulli = lift . bernoulli

instance MonadFactor m => MonadFactor (ReaderT r m) where
  score = lift . score

instance MonadMeasure m => MonadMeasure (ReaderT r m)

instance (Monoid w, MonadDistribution m) => MonadDistribution (WriterT w m) where
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical

instance (Monoid w, MonadFactor m) => MonadFactor (WriterT w m) where
  score = lift . score

instance (Monoid w, MonadMeasure m) => MonadMeasure (WriterT w m)

instance MonadDistribution m => MonadDistribution (StateT s m) where
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical
  uniformD = lift . uniformD

instance MonadFactor m => MonadFactor (StateT s m) where
  score = lift . score

instance MonadMeasure m => MonadMeasure (StateT s m)

instance MonadDistribution m => MonadDistribution (ListT m) where
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical

instance MonadFactor m => MonadFactor (ListT m) where
  score = lift . score

instance MonadMeasure m => MonadMeasure (ListT m)

instance MonadDistribution m => MonadDistribution (ContT r m) where
  random = lift random

instance MonadFactor m => MonadFactor (ContT r m) where
  score = lift . score

instance MonadMeasure m => MonadMeasure (ContT r m)

-- * Utility for deriving MonadDistribution, MonadFactor and MonadMeasure

-- | Newtype to derive 'MonadDistribution', 'MonadFactor' and 'MonadMeasure' automatically for monad transformers.
--
-- The typical usage is with the `StandaloneDeriving` and `DerivingVia` extensions.
-- For example, to derive all instances for the 'IdentityT' transformer, one writes:
--
-- @
-- deriving via (MonadMeasureTrans IdentityT m) instance MonadDistribution m => MonadDistribution (IdentityT m)
-- deriving via (MonadMeasureTrans IdentityT m) instance MonadFactor m => MonadFactor (IdentityT m)
-- instance MonadMeasure m => MonadMeasure (IdentityT m)
-- @
-- (The final 'MonadMeasure' could also be derived `via`, but this isn't necessary because it doesn't contain any methods.)
newtype MonadMeasureTrans (t :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) a = MonadMeasureTrans {getMonadMeasureTrans :: t m a}
  deriving (Functor, Applicative, Monad)

instance MonadTrans t => MonadTrans (MonadMeasureTrans t) where
  lift = MonadMeasureTrans . lift

instance (MonadTrans t, MonadDistribution m, Monad (t m)) => MonadDistribution (MonadMeasureTrans t m) where
  random = lift random
  uniform = (lift .) . uniform
  normal = (lift .) . normal
  gamma = (lift .) . gamma
  beta = (lift .) . beta
  bernoulli = lift . bernoulli
  categorical = lift . categorical
  logCategorical = lift . logCategorical
  uniformD = lift . uniformD
  geometric = lift . geometric
  poisson = lift . poisson
  dirichlet = lift . dirichlet

instance (MonadFactor m, MonadTrans t, Monad (t m)) => MonadFactor (MonadMeasureTrans t m) where
  score = lift . score

instance (MonadDistribution m, MonadFactor m, MonadTrans t, Monad (t m)) => MonadMeasure (MonadMeasureTrans t m)
