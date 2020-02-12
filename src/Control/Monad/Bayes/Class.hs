{-|
Module      : Control.Monad.Bayes.Class
Description : Types for probabilistic modelling
Copyright   : (c) Adam Scibior, 2015-2020
License     : MIT
Maintainer  : leonhard.markert@tweag.io
Stability   : experimental
Portability : GHC

This module defines 'MonadInfer', which can be used to represent a simple model
like the following:

@
import Control.Monad (when)
import Control.Monad.Bayes.Class

model :: MonadInfer m => m Bool
model = do
  rain <- bernoulli 0.3
  sprinkler <-
    bernoulli $
    if rain
      then 0.1
      else 0.4
  let wetProb =
    case (rain, sprinkler) of
      (True,  True)  -> 0.98
      (True,  False) -> 0.80
      (False, True)  -> 0.90
      (False, False) -> 0.00
  score wetProb
  return rain
@
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

-- | Monads that can draw random variables.
class Monad m => MonadSample m where
  -- | Draw from a uniform distribution.
  random :: m Double -- ^ \(\sim \mathcal{U}(0, 1)\)

  -- | Draw from a uniform distribution.
  uniform ::
       Double -- ^ lower bound a
    -> Double -- ^ upper bound b
    -> m Double -- ^ \(\sim \mathcal{U}(a, b)\).
  uniform a b = draw (uniformDistr a b)

  -- | Draw from a normal distribution.
  normal ::
       Double -- ^ mean μ
    -> Double -- ^ standard deviation σ
    -> m Double -- ^ \(\sim \mathcal{N}(\mu, \sigma^2)\)
  normal m s = draw (normalDistr m s)

  -- | Draw from a gamma distribution.
  gamma ::
       Double -- ^ shape k
    -> Double -- ^ scale θ
    -> m Double -- ^ \(\sim \Gamma(k, \theta)\)
  gamma shape scale = draw (gammaDistr shape scale)

  -- | Draw from a beta distribution.
  beta ::
       Double -- ^ shape α
    -> Double -- ^ shape β
    -> m Double -- ^ \(\sim \mathrm{Beta}(\alpha, \beta)\)
  beta a b = draw (betaDistr a b)

  -- | Draw from a Bernoulli distribution.
  bernoulli ::
       Double -- ^ probability p
    -> m Bool -- ^ \(\sim \mathrm{B}(1, p)\)
  bernoulli p = fmap (< p) random

  -- | Draw from a categorical distribution.
  categorical ::
       Vector v Double
    => v Double -- ^ event probabilities
    -> m Int -- ^ outcome category
  categorical ps = fromPMF (ps !)

  -- | Draw from a categorical distribution in the log domain.
  logCategorical ::
       (Vector v (Log Double), Vector v Double)
    => v (Log Double) -- ^ event probabilities
    -> m Int -- ^ outcome category
  logCategorical = categorical . VG.map (exp . ln)

  -- | Draw from a discrete uniform distribution.
  uniformD ::
       [a] -- ^ observable outcomes @xs@
    -> m a -- ^ \(\sim \mathcal{U}\{\mathrm{xs}\}\)
  uniformD xs = do
    let n = Prelude.length xs
    i <- categorical $ V.replicate n (1 / fromIntegral n)
    return (xs !! i)

  -- | Draw from a geometric distribution.
  geometric ::
       Double -- ^ success rate p
    -> m Int -- ^ \(\sim\) number of failed Bernoulli trials with success probability p before first success
  geometric = discrete . geometric0

  -- | Draw from a Poisson distribution.
  poisson ::
       Double -- ^ parameter λ
    -> m Int -- ^ \(\sim \mathrm{Pois}(\lambda)\)
  poisson = discrete . Poisson.poisson

  -- | Draw from a Dirichlet distribution.
  dirichlet ::
       Vector v Double
    => v Double -- ^ concentration parameters @as@
    -> m (v Double) -- ^ \(\sim \mathrm{Dir}(\mathrm{as})\)
  dirichlet as = do
    xs <- VG.mapM (`gamma` 1) as
    let s = VG.sum xs
    let ys = VG.map (/ s) xs
    return ys

-- | Draw from a continuous distribution using the inverse cumulative density
-- function.
draw :: (ContDistr d, MonadSample m) => d -> m Double
draw d = fmap (quantile d) random

-- | Draw from a discrete distribution using a sequence of draws from
-- Bernoulli.
fromPMF :: MonadSample m => (Int -> Double) -> m Int
fromPMF p = f 0 1 where
  f i r = do
    when (r < 0) $ error "fromPMF: total PMF above 1"
    let q = p i
    when (q < 0 || q > 1) $ error "fromPMF: invalid probability value"
    b <- bernoulli (q / r)
    if b then pure i else f (i+1) (r-q)

-- | Draw from a discrete distributions using the probability mass function.
discrete :: (DiscreteDistr d, MonadSample m) => d -> m Int
discrete = fromPMF . probability

-- | Monads that can score different execution paths.
class Monad m => MonadCond m where
  -- | Record a likelihood.
  score ::
       Log Double -- ^ likelihood of the execution path
    -> m ()

-- | Synonym for 'score'.
factor ::
     MonadCond m
  => Log Double -- ^ likelihood of the execution path
  -> m ()
factor = score

-- | Hard conditioning.
condition :: MonadCond m => Bool -> m ()
condition b = score $ if b then 1 else 0

-- | Monads that support both sampling and scoring.
class (MonadSample m, MonadCond m) => MonadInfer m

-- | Probability density function of the normal distribution.
normalPdf ::
     Double -- ^ mean μ
  -> Double -- ^ standard deviation σ
  -> Double -- ^ sample x
  -> Log Double -- ^ relative likelihood of observing sample x in \(\mathcal{N}(\mu, \sigma^2)\)
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
