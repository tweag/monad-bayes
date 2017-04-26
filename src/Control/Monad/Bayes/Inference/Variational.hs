{-|
Module      : Control.Monad.Bayes.Inference.Variational
Description : Variational inference algorithms
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Inference.Variational (
  module Numeric.Optimization.SGD,
  elbo,
  elboGrad,
  optimizeELBO,
  advi,
  adviSet
) where

import Prelude hiding (zip, splitAt, length, map)

import Debug.Trace (traceM)

import Data.Bifunctor (second)
import Data.Reflection (Reifies)
import Data.Vector hiding ((++))
import qualified Data.Foldable as Fold
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse (Tape)

import Numeric.LogDomain
import Numeric.Optimization.SGD
import Statistics.Distribution.Polymorphic.Unconstrained
import Control.Monad.Bayes.Simple hiding (Parametric)
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Reparametrized
import Control.Monad.Bayes.Parametric
import Control.Monad.Bayes.MeanField as MF

-- | Return a value and a gradient in an arbitrary functor and combine the gradient with input using the given function.
-- This function is exposed by AD library as 'jacobianWith'' since it corresponds to the Jacobian if the functor
-- used is a collection.
-- Here a random sampling functor is used to obtain stochastic gradient, so we introduce this alias to avoid confusion.
gradFWith' :: (Traversable f, Functor g, Num a)
           => (a -> a -> b)
           -> (forall s. Reifies s Tape => f (Reverse s a) -> g (Reverse s a))
           -> f a -> g (a, f b)
gradFWith' = jacobianWith'

-- | Estimator for evidence lower bound.
elbo :: (HasCustomReal m, Functor m) => Weighted m a -> m (CustomReal m)
elbo = fmap (toLog . snd) . runWeighted

-- | Stochastic gradient of 'elbo' using the reparametrization trick.
-- Returns a tuple where the first element is the value of ELBO and the second
-- is a traversable structure where each element is a tuple containing the argument the the gradient with respect to it.
elboGrad :: (Traversable t, HasCustomReal m, Monad m)
         => (forall s. Reifies s Tape => Parametric (t (Reverse s (CustomReal m))) (Weighted (Reparametrized s m)) a)
         -> t (CustomReal m) -> m (CustomReal m, t (CustomReal m, CustomReal m))
elboGrad model xs = do
  traceM $ "input: " ++ show (Fold.toList $ fmap fromCustomReal xs)
  (target , xsGrad) <- (gradFWith' (,) $ reparametrize . elbo . withParam model) xs
  traceM $ "elbo: " ++ show (fromCustomReal target)
  traceM $ "gradient: " ++ show (Fold.toList $ fmap (fromCustomReal . snd) xsGrad)
  return (target, xsGrad)

-- \ Find parameters that optimize ELBO using stochastic gradient descent.
optimizeELBO :: (Traversable t, HasCustomReal m, Monad m)
             => (forall s. Reifies s Tape => Parametric (t (Reverse s (CustomReal m))) (Weighted (Reparametrized s m)) a) -- ^ model
             -> SGDParam (CustomReal m) -- ^ optimization parameters
             -> t (CustomReal m) -- ^ initial values of parameters
             -> m (t (CustomReal m))
optimizeELBO model optParam initial = sga optParam (fmap snd . elboGrad model) initial

-- | Reparametrize from a vector of (mean, stddev) to twice as long vector of unconstrained real numbers.
reshapeParam :: (Ord a, Floating a) => Parametric (Vector (a, a)) m b -> Parametric (Vector a) m b
reshapeParam m = parametric (withParam m . split) where
  split v = uncurry zip $ second (map (inverseTransformConstraints (gammaDist 1 1))) $ splitAt (length v `div` 2) v

advi :: (MonadDist m)
     => (forall s. (Reifies s Tape) => MeanFieldNormal (Weighted (Reparametrized s m)) a) -- ^ model
     -> SGDParam (CustomReal m)
     -> Vector (CustomReal m)
     -> m (Vector (CustomReal m))
advi m sgdParam initParam = optimizeELBO (reshapeParam $ meanFieldNormal m) sgdParam initParam

adviSet :: MonadDist m
        => MeanFieldNormal (Weighted m) a
        -> Vector (CustomReal m)
        -> m a
adviSet model bestParam = prior $ (withParam $ reshapeParam $ meanFieldNormal model) bestParam
