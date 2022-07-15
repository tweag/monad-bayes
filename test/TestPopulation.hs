{-# LANGUAGE Trustworthy #-}

module TestPopulation (weightedSampleSize, popSize, manySize, sprinkler, sprinklerExact, transCheck1, transCheck2, resampleCheck, popAvgCheck) where

import Control.Monad.Bayes.Class (MonadInfer, MonadSample)
import Control.Monad.Bayes.Enumerator (enumerate, expectation)
import Control.Monad.Bayes.Population as Population
  ( Population,
    collapse,
    popAvg,
    pushEvidence,
    resampleMultinomial,
    runPopulation,
    spawn,
  )
import Control.Monad.Bayes.Sampler (sampleIOfixed)
import Data.AEq (AEq ((~==)))
import Sprinkler (soft)

weightedSampleSize :: MonadSample m => Population m a -> m Int
weightedSampleSize = fmap length . runPopulation

popSize :: IO Int
popSize = sampleIOfixed $ weightedSampleSize $ spawn 5 >> sprinkler

manySize :: IO Int
manySize = sampleIOfixed $ weightedSampleSize $ spawn 5 >> sprinkler >> spawn 3

sprinkler :: MonadInfer m => m Bool
sprinkler = Sprinkler.soft

sprinklerExact :: [(Bool, Double)]
sprinklerExact = enumerate Sprinkler.soft

-- all_check = (mass (Population.all id (spawn 2 >> sprinkler)) True) ~== 0.09

transCheck1 :: Bool
transCheck1 =
  enumerate (collapse sprinkler)
    ~== sprinklerExact

transCheck2 :: Bool
transCheck2 =
  enumerate (collapse (spawn 2 >> sprinkler))
    ~== sprinklerExact

resampleCheck :: Int -> Bool
resampleCheck n =
  (enumerate . collapse . resampleMultinomial) (spawn n >> sprinkler)
    ~== sprinklerExact

popAvgCheck :: Bool
popAvgCheck = expectation f Sprinkler.soft ~== expectation id (popAvg f $ pushEvidence Sprinkler.soft)
  where
    f True = 10
    f False = 4
