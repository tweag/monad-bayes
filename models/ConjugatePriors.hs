{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module ConjugatePriors where

import Control.Applicative (Applicative (liftA2))
import Control.Foldl (fold)
import Control.Foldl qualified as F
import Control.Monad.Bayes.Class
    ( 
      normalPdf,
      MonadInfer,
      MonadSample(beta, bernoulli, gamma, normal) )
import Data.Profunctor (Profunctor (lmap))
import Numeric.Log (Log (Exp))
import Prelude
import Control.Monad.Bayes.Class (Bayesian(..))



type GammaParams = (Double, Double)
type BetaParams = (Double, Double)
type NormalParams = (Double, Double)

-- | Posterior on the precision of the normal after the points are observed
gammaNormalAnalytic ::
  (MonadInfer m, Foldable t, Functor t) =>
  GammaParams -> t Double -> m Double

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
betaBernoulliAnalytic (a, b) points = beta a' b'
  where
    (n, s) = fold (liftA2 (,) F.length (lmap (\case True -> 1; False -> 0) F.sum)) points
    a' = a + s
    b' = b + fromIntegral n - s

bernoulliPdf :: Floating a => a -> Bool -> Log a
bernoulliPdf p x = let numBool = if x then 1.0 else 0 in Exp $ log (p ** numBool * (1 - p) ** (1 - numBool))

betaBernoulli' :: MonadInfer m => (Double, Double) -> Bayesian m Double Bool
betaBernoulli' (a,b) = Bayesian (beta a b) bernoulli bernoulliPdf

normalNormal' :: MonadInfer m => Double -> (Double, Double) -> Bayesian m Double Double
normalNormal' var (mu0, var0) = Bayesian (normal mu0 (sqrt var0)) (`normal` (sqrt var)) (`normalPdf` (sqrt var))

gammaNormal' :: MonadInfer m => (Double, Double) -> Bayesian m Double Double
gammaNormal' (a,b) = Bayesian (gamma a (recip b)) (normal 0 . sqrt . recip) (normalPdf 0 . sqrt . recip)

normalNormalAnalytic :: (MonadInfer m, Foldable t) =>
    Double -> NormalParams -> t Double -> m Double
normalNormalAnalytic sigma_2 (mu0, sigma0_2) points = normal mu' (sqrt sigma_2')
  where
    (n, s) = fold (liftA2 (,) F.length F.sum) points
    mu' = sigma_2' * (mu0 / sigma0_2 + s / sigma_2)
    sigma_2' = recip (recip sigma0_2 + fromIntegral n / sigma_2)

