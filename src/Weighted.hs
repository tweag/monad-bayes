{-# LANGUAGE
  GeneralizedNewtypeDeriving
 #-}

module Weighted (
    Weight,
    weight,
    unWeight,
    WeightedT(WeightedT),  --constructor is needed in Dist
    runWeightedT
                  ) where

import Control.Arrow (first,second)
import Data.Number.LogFloat
import Data.Monoid
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import Base

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
newtype WeightedT m a = WeightedT {toWriterT :: WriterT Weight m a}
    deriving(Functor, Applicative, Monad, MonadTrans, MonadDist)

runWeightedT :: Functor m => WeightedT m a -> m (a, LogFloat)
runWeightedT = fmap (second unWeight) . runWriterT . toWriterT

instance MonadDist m => MonadBayes (WeightedT m) where
    factor = WeightedT . tell . weight
