module TestPopulation (weightedSampleSize, popSize, manySize, sprinkler, sprinklerExact, transCheck1, transCheck2, resampleCheck, popAvgCheck) where

import Control.Monad.Bayes.Class (MonadInfer, MonadSample)
import Control.Monad.Bayes.Enumerator (enumerator, expectation)
import Control.Monad.Bayes.Population as Population
  ( Population,
    collapse,
    popAvg,
    population,
    pushEvidence,
    resampleMultinomial,
    spawn,
  )
import Control.Monad.Bayes.Sampler (sampleIOfixed)
import Data.AEq (AEq ((~==)))
import Sprinkler (soft)

weightedSampleSize :: MonadSample m => Population m a -> m Int
weightedSampleSize = fmap length . population

popSize :: IO Int
popSize = sampleIOfixed $ weightedSampleSize $ spawn 5 >> sprinkler

manySize :: IO Int
manySize = sampleIOfixed $ weightedSampleSize $ spawn 5 >> sprinkler >> spawn 3

sprinkler :: MonadInfer m => m Bool
sprinkler = Sprinkler.soft

sprinklerExact :: [(Bool, Double)]
sprinklerExact = enumerator Sprinkler.soft

transCheck1 :: Bool
transCheck1 =
  enumerator (collapse sprinkler)
    ~== sprinklerExact

transCheck2 :: Bool
transCheck2 =
  enumerator (collapse (spawn 2 >> sprinkler))
    ~== sprinklerExact

resampleCheck :: Int -> Bool
resampleCheck n =
  (enumerator . collapse . resampleMultinomial) (spawn n >> sprinkler)
    ~== sprinklerExact

popAvgCheck :: Bool
popAvgCheck = expectation f Sprinkler.soft ~== expectation id (popAvg f $ pushEvidence Sprinkler.soft)
  where
    f True = 10
    f False = 4
