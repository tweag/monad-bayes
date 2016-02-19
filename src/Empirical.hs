{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts
 #-}

module Empirical where

import Control.Monad.Trans.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans.List
import Data.Number.LogFloat as LogFloat

import Base

-- | Empirical distribution represented as a set of weighted samples.
-- Forward probabilistic computation is handled by the transformed monad,
-- while conditioning is done by updating empirical weights.
-- There is no automatic normalization or aggregation of weights.
newtype EmpiricalT m a = EmpiricalT (StateT LogFloat (ListT m) a)
    deriving (Functor, Applicative, Monad, MonadState LogFloat)

runEmpiricalT :: EmpiricalT m a -> StateT LogFloat (ListT m) a
runEmpiricalT (EmpiricalT d) = d

instance MonadTrans EmpiricalT where
    lift = EmpiricalT . lift . lift

instance MonadDist m => MonadDist (EmpiricalT m) where
    categorical = lift . categorical
    normal m s  = lift (normal m s)
    gamma a b   = lift (gamma a b)
    beta a b    = lift (beta a b)

instance MonadDist m => MonadBayes (EmpiricalT m) where
    factor w = modify (* w)

-- | Set the population size for the empirical distribution.
-- Bear in mind that invoking `population` twice in the same computation
-- leads to multiplying the number of samples.
population :: Monad m => Int -> EmpiricalT m ()
population n = EmpiricalT $ lift $ ListT $ sequence $ replicate n $ return ()

-- | A special version of fold that returns the result in the transformed monad.
fold :: Monad m => (b -> a -> b) -> b -> EmpiricalT m a -> m b
fold f z (EmpiricalT d) = fmap (foldl f z) $ runListT $ evalStateT d 1

-- | Checks if all samples of the empirical distribution satisfy a condition, using `fold`.
all :: Monad m => (a -> Bool) -> EmpiricalT m a -> m Bool
all cond d = fold (\b x -> b && cond x) True d

-- | Conversion to a categorical distribution.
-- No normalization is performed.
toCat :: Monad m => EmpiricalT m a -> m [(a,LogFloat)]
toCat (EmpiricalT d) = runListT $ runStateT d 1

-- | Resample the particles using the underlying monad.
-- Model evidence estimate is preserved in total weight.
resample :: MonadDist m => EmpiricalT m a -> EmpiricalT m a
resample d = do
  cat <- lift $ toCat d
  let evidence = LogFloat.sum $ map snd cat
  modify (* evidence) --to keep track of model evidence
  population (length cat)
  lift $ categorical $ cat
