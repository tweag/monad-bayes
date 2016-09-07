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
    WeightRecorder,
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
newtype WeightRecorder m a =
  WeightRecorder {runWeightRecorder :: Weighted m a}
    deriving(Functor, Applicative, Monad, MonadTrans)
type instance CustomReal (WeightRecorder m) = CustomReal m
deriving instance MonadDist m => MonadDist (WeightRecorder m)

instance MonadBayes m => MonadBayes (WeightRecorder m) where
  factor w = WeightRecorder (factor w >> lift (factor w))

-- | Both record weight and pass it to the underlying monad.
duplicateWeight :: WeightRecorder m a -> Weighted m a
duplicateWeight = runWeightRecorder

-- | Reset weight record to 1, not modifying the transformed monad.
resetWeightRecorder :: MonadDist m => WeightRecorder m a -> WeightRecorder m a
resetWeightRecorder = WeightRecorder . resetWeight . runWeightRecorder

mapMonadWeightRecorder :: MonadDist m => (forall a. m a -> m a) -> WeightRecorder m a -> WeightRecorder m a
mapMonadWeightRecorder t = WeightRecorder . mapMonad t . runWeightRecorder
