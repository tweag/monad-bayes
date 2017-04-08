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
  elboGrad
) where

import Numeric.AD.Mode.Reverse

import Numeric.LogDomain
import Control.Monad.Bayes.Simple hiding (Parametric)
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Reparametrized
import Control.Monad.Bayes.Parametric

-- | Estimator for evidence lower bound.
elbo :: (HasCustomReal m, Functor m) => Weighted m a -> m (CustomReal m)
elbo = fmap (toLog . snd) . runWeighted

-- | Stochastic gradient of 'elbo' using the reparametrization trick.
elboGrad :: (Traversable t, HasCustomReal m, Functor m)
         => (forall s. Weighted (Parametric (t (Reverse s (CustomReal m))) (Reparametrized s m)) a)
         -> t (CustomReal m) -> m (t (CustomReal m))
elboGrad model = gradM $ reparametrize . (withParam $ elbo model)
