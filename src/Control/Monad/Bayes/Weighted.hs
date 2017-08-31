{-|
Module      : Control.Monad.Bayes.Weighted
Description : Probability monad accumulating the likelihood
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Weighted (
    Weight,
    weight,
    unWeight,
    Weighted,
    withWeight,
    runWeighted,
    prior,
    flatten,
    hoist,
                  ) where

import Control.Arrow (second)
-- import Control.Applicative (($>))
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Trans.Writer

import Numeric.Log
import Control.Monad.Bayes.Class

-- | Representation of a weight in importance sampling algorithms.
-- Internally represented in log-domain, but semantically a non-negative real number.
-- 'Monoid' instance with respect to multiplication.
newtype Weight = Weight (Product (Log Double))
    deriving(Eq, Num, Ord, Show, Monoid)

-- | Semantic conversion.
weight :: Log Double -> Weight
weight = Weight . Product

-- | Inverse of `weight`.
unWeight :: Weight -> Log Double
unWeight (Weight (Product p)) = p

-- | Executes the program using the prior distribution, while accumulating likelihood.
newtype Weighted m a = Weighted (WriterT Weight m a)
    deriving(Functor, Applicative, Monad, MonadIO, MonadTrans, MonadSample)

instance Monad m => MonadCond (Weighted m) where
  score w = Weighted (tell $ weight w)

instance MonadSample m => MonadInfer (Weighted m)

-- | Obtain an explicit value of the likelihood for a given value.
runWeighted :: (Functor m) => Weighted m a -> m (a, Log Double)
runWeighted (Weighted m) = second unWeight <$> runWriterT m

-- | Embed a random variable with explicitly given likelihood.
--
-- > runWeighted . withWeight = id
withWeight :: (Monad m) => m (a, Log Double) -> Weighted m a
withWeight m = Weighted $ do
  (x,w) <- lift m
  tell (weight w)
  return x

-- | Discard the weight.
prior :: (Functor m) => Weighted m a -> m a
prior = fmap fst . runWeighted

-- | Combine weights from two different levels.
flatten :: Monad m => Weighted (Weighted m) a -> Weighted m a
flatten m = withWeight $ (\((x,p),q) -> (x, p*q)) <$> runWeighted (runWeighted m)

-- | Apply a transformation to the transformed monad.
hoist :: (forall x. m x -> n x) -> Weighted m a -> Weighted n a
hoist t (Weighted m) = Weighted $ mapWriterT t m
