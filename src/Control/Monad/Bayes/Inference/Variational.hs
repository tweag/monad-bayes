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
  advi
) where

import Prelude hiding (zip, splitAt, length, (++))

import Data.Reflection (Reifies)
import Data.Vector
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse (Tape)

import Numeric.LogDomain
import Numeric.Optimization.SGD
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
elboGrad :: (Traversable t, HasCustomReal m, Functor m)
         => (forall s. Reifies s Tape => Parametric (t (Reverse s (CustomReal m))) (Weighted (Reparametrized s m)) a)
         -> t (CustomReal m) -> m (CustomReal m, t (CustomReal m, CustomReal m))
elboGrad model = gradFWith' (,) $ reparametrize . elbo . withParam model

-- \ Find parameters that optimize ELBO using stochastic gradient descent.
optimizeELBO :: (Traversable t, HasCustomReal m, Monad m)
             => (forall s. Reifies s Tape => Parametric (t (Reverse s (CustomReal m))) (Weighted (Reparametrized s m)) a) -- ^ model
             -> SGDParam (CustomReal m) -- ^ optimization parameters
             -> t (CustomReal m) -- ^ initial values of parameters
             -> m (t (CustomReal m))
optimizeELBO model optParam initial = sga optParam (fmap snd . elboGrad model) initial

reshapeParam :: Parametric (Vector (a, a)) m b -> Parametric (Vector a) m b
reshapeParam m = parametric (withParam m . split) where
  split v = uncurry zip $ splitAt (length v `div` 2) v

advi :: (MonadDist m', MonadDist m'', CustomReal m' ~ CustomReal m'')
     => Int -- ^ number of random variables in the model
     -> (forall s. (Reifies s Tape) => MeanFieldNormal (Weighted (Reparametrized s m')) a) -- ^ model
     -> MeanFieldNormal (Weighted m'') a -- ^ same model in a different monad
     -> SGDParam (CustomReal m'')
     -> m' (m'' a)
advi n m m' sgdParam = do
  initialMeans <- replicateM n (uniform (-1) 1)
  initialStdDevs <- replicateM n (uniform 0.01 1)
  let initialParam = initialMeans ++ initialStdDevs
  bestParam <- optimizeELBO (reshapeParam $ meanFieldNormal m) sgdParam initialParam
  return $ prior $ (withParam $ reshapeParam $ meanFieldNormal m') bestParam
