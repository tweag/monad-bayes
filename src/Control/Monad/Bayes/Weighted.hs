{-# LANGUAGE
  GeneralizedNewtypeDeriving
 #-}

module Control.Monad.Bayes.Weighted (
    Weight,
    weight,
    unWeight,
    Weighted(Weighted),  --constructor is needed in Dist
    withWeight,
    runWeighted,
    WeightRecorderT,
    duplicateWeight
                  ) where

import Control.Monad.Morph
import Control.Arrow (first,second)
import Data.Number.LogFloat
import Data.Monoid
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import Control.Monad.Bayes.Class

-- | Representation of a weight in importance sampling algorithms.
-- Internally represented in log-domain, but semantically a non-negative real number.
-- 'Monoid' instance with respect to multiplication.
newtype Weight = Weight (Product LogFloat)
    deriving(Eq, Num, Ord, Show, Monoid)

weight :: LogFloat -> Weight
weight = Weight . Product

unWeight :: Weight -> LogFloat
unWeight (Weight (Product p)) = p

-- | A wrapper for 'WriterT' 'Weight' that executes the program
-- emitting the likelihood score.
newtype Weighted m a = Weighted {toWriterT :: WriterT Weight m a}
    deriving(Functor, Applicative, Monad, MonadTrans, MonadDist, MFunctor)

runWeighted :: Functor m => Weighted m a -> m (a, LogFloat)
runWeighted = fmap (second unWeight) . runWriterT . toWriterT

withWeight :: Monad m => m (a, LogFloat) -> Weighted m a
withWeight = Weighted . WriterT . fmap (second weight)

instance MonadDist m => MonadBayes (Weighted m) where
    factor = Weighted . tell . weight

-- | Similar to 'Weighted', only the weight is both recorded and passed
-- to the underlying monad. Useful for getting the exact posterior and
-- the associated likelihood.
newtype WeightRecorderT m a =
  WeightRecorderT {runWeightRecorderT :: Weighted m a}
    deriving(Functor, Applicative, Monad, MonadTrans, MonadDist)

-- | Both record weight and pass it to the underlying monad.
duplicateWeight :: MonadBayes m => WeightRecorderT m a -> Weighted m a
duplicateWeight = runWeightRecorderT

instance MonadBayes m => MonadBayes (WeightRecorderT m) where
  factor w = WeightRecorderT (factor w >> lift (factor w))
