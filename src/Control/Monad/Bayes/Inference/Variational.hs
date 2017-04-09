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
  elbo,
  elboGrad,
  -- elboWithGrad,
  optimizeELBO
) where

import Numeric.AD.Mode.Reverse

import Numeric.LogDomain
import Numeric.Optimization.SGD
import Control.Monad.Bayes.Simple hiding (Parametric)
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Reparametrized
import Control.Monad.Bayes.Parametric

-- | Estimator for evidence lower bound.
elbo :: (HasCustomReal m, Functor m) => Weighted m a -> m (CustomReal m)
elbo = fmap (toLog . snd) . runWeighted

-- | Stochastic gradient of 'elbo' using the reparametrization trick.
-- | Returns a traversable structure where each element is a tuple containing the argument the the gradient with respect to it.
elboGrad :: (Traversable t, HasCustomReal m, Functor m)
         => (forall s. Parametric (t (Reverse s (CustomReal m))) (Weighted (Reparametrized s m)) a)
         -> t (CustomReal m) -> m (t (CustomReal m, CustomReal m))
elboGrad model = gradFWith (,) $ reparametrize . elbo . withParam model

-- -- | Like 'elboGrad', but also returns the ELBO estimator.
-- elboWithGrad :: (Traversable t, HasCustomReal m, Functor m)
--              => (forall s. Weighted (Parametric (t (Reverse s (CustomReal m))) (Reparametrized s m)) a)
--              -> t (CustomReal m) -> m (CustomReal m, t (CustomReal m))
-- elboWithGrad model = gradM' $ reparametrize . (withParam $ elbo model)

-- \ Find parameters that optimize ELBO using stochastic gradient descent.
optimizeELBO :: (Traversable t, HasCustomReal m, Monad m)
             => (forall s. Parametric (t (Reverse s (CustomReal m))) (Weighted (Reparametrized s m)) a) -- ^ model
             -> SGDParam (CustomReal m) -- ^ optimization parameters
             -> t (CustomReal m) -- ^ initial values of parameters
             -> m (t (CustomReal m))
optimizeELBO model optParam initial = sga optParam (elboGrad model) initial
