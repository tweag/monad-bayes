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
    spawn,
    all,
    resampleN,
    evidence,
    collapse,
    proper,
    transform
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

-- | The number of samples used for approximation.
population :: Monad m => EmpiricalT m a -> m Int
population e = do
  zs <- runEmpiricalT e
  return (length zs)

-- | Set the number of samples for the empirical distribution.
-- Bear in mind that invoking `spawn` twice in the same computation
-- leads to multiplying the number of samples.
spawn :: MonadDist m => Int -> EmpiricalT m ()
spawn n = (EmpiricalT $ lift $ ListT $ sequence $ replicate n $ return ()) >>
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
  n <- lift (population d)
  resampleN n d

-- | Model evidence estimator, also known as pseudo-marginal likelihood.
evidence :: MonadDist m => EmpiricalT m a -> m LogFloat
evidence = fmap snd . proper

-- | Pick a sample at random from the empirical distribution,
-- according to the weights.
collapse :: MonadDist m => EmpiricalT m a -> m a
collapse = fmap fst . proper

-- | As 'resample', but with set new population size.
resampleN :: MonadDist m => Int -> EmpiricalT m a -> EmpiricalT m a
resampleN n d = do
  ys <- lift $ runEmpiricalT d
  let (xs,ws) = unzip ys
  let z = LogFloat.sum ws
  factor z
  spawn n
  ancestor <- discrete ws
  let x = xs !! ancestor
  return x

-- | Properly weighted version of 'collapse', that is returned with
-- model evidence estimator from the same run.
proper :: MonadDist m => EmpiricalT m a -> m (a,LogFloat)
proper = fmap head . runEmpiricalT . resampleN 1


-- | Pick one sample from the empirical distribution and use model evidence as a 'factor'.
transform :: (MonadDist m, MonadTrans t, MonadBayes (t m)) => EmpiricalT m a -> t m a
transform e = do
  (x,p) <- lift $ proper e
  factor p
  return x



