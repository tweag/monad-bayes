{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}

module Gamma where

import Control.Monad.Bayes.Class
import Prelude
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad (replicateM)
import Control.Foldl (mean, fold, variance)
import Control.Applicative (Applicative(liftA2))
import Control.Monad.Bayes.Weighted (runWeighted, prior)
import Control.Monad.Bayes.Traced (mh)
import Numeric.Log (Log(Exp, ln))
import qualified Control.Foldl as F
import Debug.Trace (trace)


-- | A sequence of i.i.d. normal variables with Gamma prior on precision.
points :: [Double]
points = [0.2] -- , -0.6, 0.45, -0.3]

type GammaParams = (Double, Double)

-- | Posterior on the precision of the normal after the points are observed
model, exact :: (MonadInfer m, Foldable t, Functor t) => GammaParams -> t Double -> m Double
model (a,b) points = do
  prec <- gamma a b
  let stddev = sqrt (1 / prec)
  let observe x = factor $ Exp $ log $ nrml 0 stddev x --  $ log $ normalPdf 0 stddev x
  mapM_ observe points
  return prec

nrml mu sigma x = 1 / (sigma * (sqrt (2 * pi))) * exp ((-0.5) * ((x-mu) / sigma)**2)

-- | Exact posterior for the model.
-- For derivation see Kevin Murphy's
-- "Conjugate Bayesian analysis of the Gaussian distribution"
-- section 4.
exact (a,b) points = gamma a' b'
  where
    a' = a + fromIntegral (length points) / 2
    b' = b + sum (fmap (** 2) points) / 2


type BetaParams = (Double,Double)

-- | Posterior on beta after the bernoulli sample
betaBinApprox, betaBinExact :: (MonadInfer m, Foldable t) => BetaParams -> t Double -> m Double
betaBinApprox (a,b) points = do
  p <- beta a b
  let observe x = factor $ Exp $ log (p**x*(1 - p)**(1-x))
  mapM_ observe points
  return p

betaBinExact (a,b) points = beta a' b' where
  (n, s) = fold (liftA2 (,) F.length F.sum) points
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

type NormalParams = (Double,Double)

-- | Posterior on the precision of the normal after the points are observed
normalApprox, normalExact :: (MonadInfer m, Foldable t, Functor t) => NormalParams -> t Double -> m Double
normalApprox (mu,sigma) points = do
  mu <- normal mu sigma
  let observe = factor . normalPdf mu 1
  mapM_ observe points
  return mu

-- nrml mu sigma x = (1 / (sigma * (sqrt (2 * pi)))) * exp ((-0.5) * ((x-mu) / sigma)**2)

-- | Exact posterior for the model.
-- For derivation see Kevin Murphy's
-- "Conjugate Bayesian analysis of the Gaussian distribution"
-- section 4.
normalExact (mu,sigma) points = normal mu' sigma'
  where
    var = sigma ** 2
    (n, s) = fold (liftA2 (,) F.length F.sum) points
    mu' = sigma'
      * (mu / var + s / 1)
    sigma' = recip (recip var + fromIntegral n / 1)


tg s = fold (liftA2 (,) mean variance) <$> (fmap (take 5000) . sampleIO  . prior . mh 10000) s


-- tg2 s = 
--   fold (liftA2 (,) mean variance) . fmap (\(n,w) -> n * trace (show (ln $ exp w, n)) (ln $ exp w)) <$>
--   (replicateM 100 $ (sampleIO . runWeighted) s)


-- traceIt x = trace (show x) x 