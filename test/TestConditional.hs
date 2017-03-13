{-# LANGUAGE
  GADTs
 #-}

 module TestConditional where

import Data.AEq
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe


import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Conditional
import qualified Control.Monad.Bayes.Trace as Trace

discreteWeights :: Fractional a => [a]
discreteWeights = [0.0001, 0.9999]

m :: (MonadDist m, CustomReal m ~ Double) => m (Int, Double)
m = do
  x <- discrete discreteWeights
  y <- normal 0 1
  return (x,y)

m' :: (MonadBayes m, CustomReal m ~ Double) => Conditional m (Int, Double)
m' = do
  x <- lift $ discrete discreteWeights
  y <- normal 0 1
  return (x,y)

conditional_m :: (MonadBayes m, CustomReal m ~ Double) => Maybe Int -> Maybe Double -> m (Int,Double)
conditional_m mx my = liftM2 (,) px py where
  px = case mx of
    Just x -> factor (if x == 0 then 0.0001 else if x == 1 then 0.9999 else 0)
      >> return x
    Nothing -> discrete discreteWeights
  py = case my of
    Just x -> factor (pdf (Normal 0 1) x) >> return x
    Nothing -> normal 0 1

check_conditional :: Maybe Int -> Maybe Double -> Bool
check_conditional mx my =
  enumerate (unsafeConditional m $ Trace.fromLists (catMaybes [my], catMaybes [mx])) ~==
    enumerate (conditional_m mx my)

check_empty_conditional = undefined

check_first_conditional = undefined

-- Returns Nothing when one of the values is empty
check_missing_conditional = all (isNothing . fst) (enumerate $ runMaybeT $ maybeConditional m $ Trace.fromLists ([0.3],[]))

-- Computes conditional correctly in presence of more values than required
check_longer_conditional =
  all (isNothing . fst) (enumerate $ runMaybeT $ maybeConditional m $ Trace.fromLists ([0.3,undefined], [1]))

-- Computes pseudomarginal density correctly
check_first_density =
  enumerate (fmap toLog (unsafeDensity m' $ Trace.fromLists ([0.6],[]))) ~==
    [(toLog (pdf (Normal 0 1) (0.6 :: Double)), 1)]

-- Computes joint density correctly when possible
check_joint_density_true =
  case maybeJointDensity m $ Trace.fromLists ([0.5],[1]) of
    Just x -> toLog x ~==
      toLog ((discreteWeights !! 1) * (pdf (Normal 0 1) (0.5 :: Double)))
    Nothing -> False

-- Returns Nothing when not possible to compute joint density,
-- here no Int value
check_joint_density_false =
  case maybeJointDensity m $ Trace.fromLists ([1], []) of
    Just x -> False
    Nothing -> True
