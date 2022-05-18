{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module ConjugatePriors where

import Control.Applicative (Applicative (liftA2))
import Control.Foldl (fold, mean, variance)
import qualified Control.Foldl as F
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler (sampleIO, SamplerIO)
import Control.Monad.Bayes.Traced (mh, Traced)
import Control.Monad.Bayes.Weighted (prior, runWeighted, Weighted)
import Data.Profunctor (Profunctor (lmap))
import Numeric.Log (Log (Exp, ln))
import Prelude
import Control.Monad.Bayes.Integrator (expectation)
-- import Control.Monad.Bayes.Integrator (normalize)

data Bayesian m z o = Bayesian {
  latent :: m z,
  generative :: z -> m o,
  likelihood :: z -> o -> Log Double
  }

posterior :: (MonadInfer m, Foldable f, Functor f) => Bayesian m z o -> f o -> m z
posterior Bayesian {..} os = do
  z <- latent
  factor $ product $ fmap (likelihood z) os
  return z

-- | A sequence of i.i.d. normal variables with Gamma prior on precision.
-- points :: [Double]
-- points = [0.2] -- , -0.6, 0.45, -0.3]
type GammaParams = (Double, Double)
type BetaParams = (Double, Double)
type NormalParams = (Double, Double)

-- | Posterior on the precision of the normal after the points are observed
gammaNormalAnalytic ::
  (MonadInfer m, Foldable t, Functor t) =>
  GammaParams -> t Double -> m Double
-- gammaNormal (a, b) points = do
--   prec <- gamma a (recip b)
--   let stddev = sqrt (1 / prec)
--   let observe = factor . normalPdf 0 stddev
--   mapM_ observe points
--   return prec

-- nrml mu sigma x = 1 / (sigma * (sqrt (2 * pi))) * exp ((-0.5) * ((x - mu) / sigma) ** 2)

-- | Exact posterior for the model.
-- For derivation see Kevin Murphy's
-- "Conjugate Bayesian analysis of the Gaussian distribution"
-- section 4.
gammaNormalAnalytic (a, b) points = gamma a' (recip b')
  where
    a' = a + fromIntegral (length points) / 2
    b' = b + sum (fmap (** 2) points) / 2


-- | Posterior on beta after the bernoulli sample
betaBernoulliAnalytic :: (MonadInfer m, Foldable t) => BetaParams -> t Bool -> m Double
-- betaBernoulli (a, b) points = do
--   p <- beta a b
--   let observe x = factor $ Exp $ log (p ** x * (1 - p) ** (1 - x))
--   mapM_ observe points
--   return p
betaBernoulliAnalytic (a, b) points = beta a' b'
  where
    (n, s) = fold (liftA2 (,) F.length (lmap (\case True -> 1; False -> 0) F.sum)) points
    a' = a + s
    b' = b + fromIntegral n - s

simpleTest1, simpleTest2 :: MonadInfer m => m Double
simpleTest1 = do
  x <- random
  condition (x > 0.5)
  return x
simpleTest2 = do
  x <- random
  return ((x + 1) / 2)





bernoulliPdf :: Floating a => a -> Bool -> Log a
bernoulliPdf p x = let numBool = if x then 1.0 else 0 in Exp $ log (p ** numBool * (1 - p) ** (1 - numBool))

betaBernoulli' :: MonadInfer m => (Double, Double) -> Bayesian m Double Bool
betaBernoulli' (a,b) = Bayesian (beta a b) bernoulli bernoulliPdf

normalNormal' :: MonadInfer m => Double -> (Double, Double) -> Bayesian m Double Double
normalNormal' var (mu0, var0) = Bayesian (normal mu0 (sqrt var0)) (`normal` sqrt var) (`normalPdf` sqrt var)

gammaNormal' :: MonadInfer m => (Double, Double) -> Bayesian m Double Double
gammaNormal' (a,b) = Bayesian (gamma a (recip b)) (normal 0 . sqrt . recip) (normalPdf 0 . sqrt . recip)

-- nrml mu sigma x = (1 / (sigma * (sqrt (2 * pi)))) * exp ((-0.5) * ((x-mu) / sigma)**2)

-- | Exact posterior for the model.
-- For derivation see Kevin Murphy's
-- "Conjugate Bayesian analysis of the Gaussian distribution"
-- section 4.
-- | Posterior on the precision of the normal after the points are observed
normalNormalAnalytic ::
    (MonadInfer m, Foldable t, Functor t) =>
    Double -> NormalParams ->
    t Double -> m Double
-- normalNormal sigma_2 (mu0, sigma0) points = do
--   mu <- normal mu0 sigma0
--   let observe = factor . normalPdf mu (sqrt sigma_2)
--   mapM_ observe points
--   return mu
normalNormalAnalytic sigma_2 (mu0, sigma0_2) points = normal mu' (sqrt sigma_2')
  where
    (n, s) = fold (liftA2 (,) F.length F.sum) points
    mu' = sigma_2' * (mu0 / sigma0_2 + s / sigma_2)
    sigma_2' = recip (recip sigma0_2 + fromIntegral n / sigma_2)



-- testC = do
--   print $ expectation $ normalize $ normalNormalAnalytic 1 (1,1) [-6.0,-19.15,9.811,-19.0923,-15.1351,12.8544,13.48199,7.17525]
--   print $ expectation $ normalize $ posterior (normalNormal' 1 (1,1)) [-6.0,-19.15,9.811,-19.0923,-15.1351,12.8544,13.48199,7.17525]
  -- normalNormalAnalytic (1) (1,1) [-1]