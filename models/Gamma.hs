{-# LANGUAGE TypeFamilies #-}

module Gamma where

import Control.Monad.Bayes.Class
import Prelude 
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad (replicateM)
import Control.Foldl (mean, fold, variance)
import Control.Applicative (Applicative(liftA2))
import Control.Monad.Bayes.Weighted (runWeighted, prior)
import Control.Monad.Bayes.Traced (mh)


-- | A sequence of i.i.d. normal variables with Gamma prior on precision.
points :: [Double]
points = [0.8, 0.2, -0.6, 0.45, -0.3]

-- | Posterior on the precision of the normal after the points are observed
model :: MonadInfer m => m Double
model = do
  prec <- gamma 1 1
  let stddev = sqrt (1 / prec)
  let observeWithNoise x = factor $ normalPdf 0 stddev x
  mapM_ observeWithNoise points
  return prec

-- | Exact posterior for the model.
-- For derivation see Kevin Murphy's
-- "Conjugate Bayesian analysis of the Gaussian distribution"
-- section 4.
exact :: MonadSample m => m Double
exact = gamma a b
  where
    a = 1 + fromIntegral (length points) / 2
    b = 1 + sum (map (^ (2 :: Int)) points) / 2

tg = fold (liftA2 (,) mean variance) <$> (fmap (take 5000) . sampleIO  . prior . mh 20000) (exact)
tg2 = fold (liftA2 (,) mean variance) <$> (fmap (take 5000) . sampleIO  . prior . mh 20000) (model)