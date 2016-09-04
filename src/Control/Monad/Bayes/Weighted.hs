{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  TypeFamilies,
  StandaloneDeriving,
  FlexibleContexts,
  RankNTypes
 #-}

module Control.Monad.Bayes.Weighted (
    Weight,
    weight,
    unWeight,
    Weighted,
    withWeight,
    runWeighted,
    resetWeight,
    mapMonad,
    WeightRecorderT,
    duplicateWeight,
    resetWeightRecorder,
    mapMonadWeightRecorder
                  ) where

import Control.Arrow (first,second)
import Data.Monoid
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

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
newtype Weighted m a = Weighted {toStateT :: StateT (Weight (CustomReal m)) m a}
    deriving(Functor, Applicative, Monad)

type instance CustomReal (Weighted m) = CustomReal m

instance MonadTrans Weighted where
  lift = Weighted . lift

instance MonadDist m => MonadDist (Weighted m) where
  primitive = lift . primitive

instance MonadDist m => MonadBayes (Weighted m) where
  factor w = Weighted $ modify (* weight w)

runWeighted :: MonadDist m => Weighted m a -> m (a, LogDomain (CustomReal m))
runWeighted = fmap (second unWeight) . (`runStateT` 1) . toStateT

withWeight :: MonadDist m => m (a, LogDomain (CustomReal m)) -> Weighted m a
withWeight m = Weighted $ do
  (x,w) <- lift m
  put $ weight w
  return x

-- | Reset weight to 1.
resetWeight :: MonadDist m => Weighted m a -> Weighted m a
resetWeight (Weighted m) = Weighted $ m >>= \x -> put 1 >> return x

mapMonad :: MonadDist m => (forall a. m a -> m a) -> Weighted m a -> Weighted m a
mapMonad t = Weighted . mapStateT t . toStateT

-- | Similar to 'Weighted', only the weight is both recorded and passed
-- to the underlying monad. Useful for getting the exact posterior and
-- the associated likelihood.
newtype WeightRecorderT m a =
  WeightRecorderT {runWeightRecorderT :: Weighted m a}
    deriving(Functor, Applicative, Monad, MonadTrans)
type instance CustomReal (WeightRecorderT m) = CustomReal m
deriving instance MonadDist m => MonadDist (WeightRecorderT m)

instance MonadBayes m => MonadBayes (WeightRecorderT m) where
  factor w = WeightRecorderT (factor w >> lift (factor w))

-- | Both record weight and pass it to the underlying monad.
duplicateWeight :: WeightRecorderT m a -> Weighted m a
duplicateWeight = runWeightRecorderT

-- | Reset weight record to 1, not modifying the transformed monad.
resetWeightRecorder :: MonadDist m => WeightRecorderT m a -> WeightRecorderT m a
resetWeightRecorder = WeightRecorderT . resetWeight . runWeightRecorderT

mapMonadWeightRecorder :: MonadDist m => (forall a. m a -> m a) -> WeightRecorderT m a -> WeightRecorderT m a
mapMonadWeightRecorder t = WeightRecorderT . mapMonad t . runWeightRecorderT
