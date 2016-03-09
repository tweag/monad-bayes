{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts,
  ExistentialQuantification,
  RankNTypes
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
import Data.Monoid
import qualified Data.Foldable as Fold

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
population :: MonadDist m => Int -> EmpiricalT m ()
population n = (EmpiricalT $ lift $ ListT $ sequence $ replicate n $ return ()) >>
               factor (1 / fromIntegral n)

-- | A special version of fold that returns the result in the transformed monad.
fold :: (Monoid a, Monad m) => EmpiricalT m a -> m a
fold = fmap (Fold.fold . map fst) . runEmpiricalT

-- | Checks if all samples of the empirical distribution satisfy a condition, using `fold`.
all :: Monad m => (a -> Bool) -> EmpiricalT m a -> m Bool
all cond = fmap getAll . fold . fmap (All . cond)

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

-- | Model evidence estimator, also known as pseudo-marginal likelihood.
evidence :: Monad m => EmpiricalT m a -> m LogFloat
evidence e = do
  zs <- runEmpiricalT e
  return $ LogFloat.sum $ map snd zs

-- | Pick a sample at random from the empirical distribution,
-- according to the weights.
collapse :: MonadDist m => EmpiricalT m a -> m a
collapse e = do
  zs <- runEmpiricalT e
  let (xs,ws) = unzip zs
  ancestor <- discrete ws
  return (xs !! ancestor)

-- | Properly weighted version of 'collapse', that is returned with
-- model evidence estimator from the same run.
proper :: MonadDist m => EmpiricalT m a -> m (a,LogFloat)
proper e = liftM2 (,) (collapse e) (evidence e)

-- | Transforms a model into one with identical marginal, but with
-- SMC run as auxiliary latent variables.
transform :: (MonadDist m, MonadBayes n) => (forall a. m a -> n a) -> EmpiricalT m a -> n a
transform conv e = do
  (x,p) <- conv $ proper e
  factor p
  return x
