{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TestInference where

import ConjugatePriors
  ( betaBernoulli',
    betaBernoulliAnalytic,
    gammaNormal',
    gammaNormalAnalytic,
    normalNormal',
    normalNormalAnalytic,
  )
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class (MonadMeasure, posterior)
import Control.Monad.Bayes.Enumerator (enumerator)
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Integrator (normalize)
import Control.Monad.Bayes.Integrator qualified as Integrator
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler.Strict (SamplerT, sampleIOfixed)
import Control.Monad.Bayes.Sampler.Strict qualified as Sampler
import Control.Monad.Bayes.Weighted (WeightedT)
import Control.Monad.Bayes.Weighted qualified as WeightedT
import Data.AEq (AEq ((~==)))
import Numeric.Log (Log)
import Sprinkler (soft)
import System.Random.Stateful (IOGenM, StdGen)

sprinkler :: (MonadMeasure m) => m Bool
sprinkler = Sprinkler.soft

-- | Count the number of particles produced by SMC
checkParticles :: Int -> Int -> IO Int
checkParticles observations particles =
  sampleIOfixed (fmap length (runPopulationT $ smc SMCConfig {numSteps = observations, numParticles = particles, resampler = resampleMultinomial} Sprinkler.soft))

checkParticlesSystematic :: Int -> Int -> IO Int
checkParticlesSystematic observations particles =
  sampleIOfixed (fmap length (runPopulationT $ smc SMCConfig {numSteps = observations, numParticles = particles, resampler = resampleSystematic} Sprinkler.soft))

checkParticlesStratified :: Int -> Int -> IO Int
checkParticlesStratified observations particles =
  sampleIOfixed (fmap length (runPopulationT $ smc SMCConfig {numSteps = observations, numParticles = particles, resampler = resampleStratified} Sprinkler.soft))

checkTerminateSMC :: IO [(Bool, Log Double)]
checkTerminateSMC = sampleIOfixed (runPopulationT $ smc SMCConfig {numSteps = 2, numParticles = 5, resampler = resampleMultinomial} sprinkler)

checkPreserveSMC :: Bool
checkPreserveSMC =
  (enumerator . collapse . smc SMCConfig {numSteps = 2, numParticles = 2, resampler = resampleMultinomial}) sprinkler
    ~== enumerator sprinkler

expectationNearNumeric ::
  WeightedT Integrator.Integrator Double ->
  WeightedT Integrator.Integrator Double ->
  Double
expectationNearNumeric x y =
  let e1 = Integrator.expectation $ normalize x
      e2 = Integrator.expectation $ normalize y
   in (abs (e1 - e2))

expectationNearSampling ::
  WeightedT (SamplerT (IOGenM StdGen) IO) Double ->
  WeightedT (SamplerT (IOGenM StdGen) IO) Double ->
  IO Double
expectationNearSampling x y = do
  e1 <- sampleIOfixed $ fmap Sampler.sampleMean $ replicateM 10 $ WeightedT.runWeightedT x
  e2 <- sampleIOfixed $ fmap Sampler.sampleMean $ replicateM 10 $ WeightedT.runWeightedT y
  return (abs (e1 - e2))

testNormalNormal :: [Double] -> IO Bool
testNormalNormal n = do
  let e =
        expectationNearNumeric
          (posterior (normalNormal' 1 (1, 10)) n)
          (normalNormalAnalytic 1 (1, 10) n)
  return (e < 1e-0)

testGammaNormal :: [Double] -> IO Bool
testGammaNormal n = do
  let e =
        expectationNearNumeric
          (posterior (gammaNormal' (1, 1)) n)
          (gammaNormalAnalytic (1, 1) n)
  return (e < 1e-1)

testBetaBernoulli :: [Bool] -> IO Bool
testBetaBernoulli bs = do
  let e =
        expectationNearNumeric
          (posterior (betaBernoulli' (1, 1)) bs)
          (betaBernoulliAnalytic (1, 1) bs)
  return (e < 1e-1)
