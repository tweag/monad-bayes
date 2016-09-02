{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  TypeFamilies,
  StandaloneDeriving,
  FlexibleContexts
 #-}

module Control.Monad.Bayes.Weighted (
    Weight,
    weight,
    unWeight,
    Weighted(Weighted),  --constructor is needed in Dist
    withWeight,
    runWeighted,
    WeightRecorderT(WeightRecorderT), -- constructor used in Trace
    runWeightRecorderT,               -- destructor  used in Trace
    duplicateWeight
                  ) where

import Control.Arrow (first,second)
import Data.Monoid
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Class

-- | Representation of a weight in importance sampling algorithms.
-- Internally represented in log-domain, but semantically a non-negative real number.
-- 'Monoid' instance with respect to multiplication.
newtype Weight r = Weight (Product (LogDomain r))
    deriving(Eq, Num, Ord, Show, Monoid)

weight :: LogDomain r -> Weight r
weight = Weight . Product

unWeight :: Weight r -> LogDomain r
unWeight (Weight (Product p)) = p

-- | A wrapper for 'WriterT' ('Weight' 'r') that executes the program
-- emitting the likelihood score.
newtype Weighted m a = Weighted {toWriterT :: WriterT (Weight (CustomReal m)) m a}
    deriving(Functor)

deriving instance MonadDist m => Applicative (Weighted m)
deriving instance MonadDist m => Monad (Weighted m)

type instance CustomReal (Weighted m) = CustomReal m

runWeighted :: MonadDist m => Weighted m a -> m (a, LogDomain (CustomReal m))
runWeighted = fmap (second unWeight) . runWriterT . toWriterT

withWeight :: MonadDist m => m (a, LogDomain (CustomReal m)) -> Weighted m a
withWeight = Weighted . WriterT . fmap (second weight)

instance MonadTrans Weighted where
  lift = undefined -- Weighted . lift

instance MonadDist m => MonadDist (Weighted m) where
  primitive = lift . primitive

instance MonadDist m => MonadBayes (Weighted m) where
    factor = Weighted . tell . weight

-- | Similar to 'Weighted', only the weight is both recorded and passed
-- to the underlying monad. Useful for getting the exact posterior and
-- the associated likelihood.
newtype WeightRecorderT m a =
  WeightRecorderT {runWeightRecorderT :: Weighted m a}
    deriving(Functor)--, Applicative, Monad, MonadTrans, MonadDist)
deriving instance MonadDist m => Applicative (WeightRecorderT m)
deriving instance MonadDist m => Monad (WeightRecorderT m)
type instance CustomReal (WeightRecorderT m) = CustomReal m

-- | Both record weight and pass it to the underlying monad.
duplicateWeight :: MonadBayes m => WeightRecorderT m a -> Weighted m a
duplicateWeight = runWeightRecorderT

instance MonadTrans (WeightRecorderT) where
  lift = undefined

instance MonadDist m => MonadDist (WeightRecorderT m) where
  primitive = WeightRecorderT . primitive

instance MonadBayes m => MonadBayes (WeightRecorderT m) where
  factor w = WeightRecorderT (factor w >> lift (factor w))
