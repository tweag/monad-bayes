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
  optimizeELBO,
  advi
) where

import Prelude hiding (zip, splitAt, length, (++))

import Data.Vector
import Numeric.AD.Mode.Reverse

import Numeric.LogDomain
import Numeric.Optimization.SGD
import Control.Monad.Bayes.Simple hiding (Parametric)
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Reparametrized
import Control.Monad.Bayes.Parametric
import Control.Monad.Bayes.MeanField as MF
import Control.Monad.Bayes.Constraint

-- | Estimator for evidence lower bound.
elbo :: (HasCustomReal m, Functor m) => Weighted m a -> m (CustomReal m)
elbo = fmap (toLog . snd) . runWeighted

-- | Stochastic gradient of 'elbo' using the reparametrization trick.
-- Returns a tuple where the first element is the value of ELBO and the second
-- is a traversable structure where each element is a tuple containing the argument the the gradient with respect to it.
elboGrad :: (Traversable t, HasCustomReal m, Functor m)
         => (forall s. Parametric (t (Reverse s (CustomReal m))) (Weighted (Reparametrized s m)) a)
         -> t (CustomReal m) -> m (CustomReal m, t (CustomReal m, CustomReal m))
elboGrad model = gradFWith' (,) $ reparametrize . elbo . withParam model

-- \ Find parameters that optimize ELBO using stochastic gradient descent.
optimizeELBO :: (Traversable t, HasCustomReal m, Monad m)
             => (forall s. Parametric (t (Reverse s (CustomReal m))) (Weighted (Reparametrized s m)) a) -- ^ model
             -> SGDParam (CustomReal m) -- ^ optimization parameters
             -> t (CustomReal m) -- ^ initial values of parameters
             -> m (t (CustomReal m))
optimizeELBO model optParam initial = sga optParam (fmap snd . elboGrad model) initial

reshapeParam :: Parametric (Vector (a, a)) m b -> Parametric (Vector a) m b
reshapeParam m = parametric (withParam m . split) where
  split v = uncurry zip $ splitAt (length v `div` 2) v

advi :: (MonadDist m', MonadDist m'', CustomReal m' ~ CustomReal m'')
     => Int -- ^ number of random variables in the model
     -> (forall s. MeanFieldNormal (Constraint (Weighted (Reparametrized s m'))) a) -- ^ model
     -> MeanFieldNormal (Constraint (Weighted m'')) a -- ^ same model in a different monad
     -> SGDParam (CustomReal m'')
     -> m' (m'' a)
advi n m m' sgdParam = do
  initialMeans <- replicateM n (uniform (-1) 1)
  initialStdDevs <- replicateM n (uniform 0.01 1)
  let initialParam = initialMeans ++ initialStdDevs
  bestParam <- optimizeELBO (reshapeParam $ meanFieldNormal $ MF.hoist unconstrain m) sgdParam initialParam
  return $ prior $ (withParam $ reshapeParam $ meanFieldNormal $ MF.hoist unconstrain m') bestParam
