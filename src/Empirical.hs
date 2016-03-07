{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts
 #-}

module Empirical (
    EmpiricalT,
    runEmpiricalT,
    population,
    all,
    resample
                 ) where

import Prelude hiding (all)

import Control.Monad.Trans.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans.List
import Data.Number.LogFloat as LogFloat

import Base
import Weighted

-- | Empirical distribution represented as a set of weighted samples.
-- Forward probabilistic computation is handled by the transformed monad,
-- while conditioning is done by updating empirical weights.
-- There is no automatic normalization or aggregation of weights.
newtype EmpiricalT m a = EmpiricalT {unEmpirical :: WeightedT (ListT m) a}
    deriving (Functor, Applicative, Monad, MonadDist, MonadBayes)

runEmpiricalT :: Functor m => EmpiricalT m a -> m [(a, LogFloat)]
runEmpiricalT = runListT . runWeightedT . unEmpirical

instance MonadTrans EmpiricalT where
    lift = EmpiricalT . lift . lift

-- | Set the population size for the empirical distribution.
-- Bear in mind that invoking `population` twice in the same computation
-- leads to multiplying the number of samples.
population :: Monad m => Int -> EmpiricalT m ()
population n = EmpiricalT $ lift $ ListT $ sequence $ replicate n $ return ()

-- | A special version of fold that returns the result in the transformed monad.
fold :: Monad m => (b -> a -> b) -> b -> EmpiricalT m a -> m b
fold f z = fmap (foldl f z . map fst) . runEmpiricalT

-- | Checks if all samples of the empirical distribution satisfy a condition, using `fold`.
all :: Monad m => (a -> Bool) -> EmpiricalT m a -> m Bool
all cond d = fold (\b x -> b && cond x) True d

-- | Resample the particles using the underlying monad.
-- Model evidence estimate is preserved in total weight.
resample :: MonadDist m => EmpiricalT m a -> EmpiricalT m a
resample d = do
  cat <- lift $ runEmpiricalT d
  let (ps,weights) = unzip cat
  let evidence = LogFloat.sum weights
  factor evidence --to keep track of model evidence
  population (length cat)
  ancestor <- discrete weights
  return (ps !! ancestor)
