module TestPopulation (weightedSampleSize, popSize, manySize, sprinkler, sprinklerExact, transCheck1, transCheck2, resampleCheck, popAvgCheck) where

import Control.Monad.Bayes.Class (MonadDistribution, MonadMeasure)
import Control.Monad.Bayes.Enumerator (enumerator, expectation)
import Control.Monad.Bayes.Population as Population
  ( PopulationT,
    collapse,
    popAvg,
    pushEvidence,
    resampleMultinomial,
    runPopulationT,
    spawn,
  )
import Control.Monad.Bayes.Sampler.Strict (sampleIOfixed)
import Data.AEq (AEq ((~==)))
import Sprinkler (soft)

weightedSampleSize :: (MonadDistribution m) => PopulationT m a -> m Int
weightedSampleSize = fmap length . runPopulationT

popSize :: IO Int
popSize =
  sampleIOfixed (weightedSampleSize $ spawn 5 >> sprinkler)

manySize :: IO Int
manySize =
  sampleIOfixed (weightedSampleSize $ spawn 5 >> sprinkler >> spawn 3)

sprinkler :: (MonadMeasure m) => m Bool
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
