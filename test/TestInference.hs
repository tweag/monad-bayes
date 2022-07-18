{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Trustworthy #-}
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
import Control.Monad.Bayes.Class (MonadInfer, posterior)
import Control.Monad.Bayes.Enumerator (enumerate)
import Control.Monad.Bayes.Inference.SMC
  ( smcMultinomial,
    smcStratified,
    smcSystematic,
  )
import Control.Monad.Bayes.Integrator (normalize)
import Control.Monad.Bayes.Integrator qualified as Integrator
import Control.Monad.Bayes.Population (collapse, runPopulation)
import Control.Monad.Bayes.Sampler (Sampler, sampleIOfixed, sampleWith)
import Control.Monad.Bayes.Sampler qualified as Sampler
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Bayes.Weighted qualified as Weighted
import Data.AEq (AEq ((~==)))
import Numeric.Log (Log)
import Sprinkler (soft)
import System.Random.Stateful (IOGenM, StdGen, mkStdGen, newIOGenM)

sprinkler :: MonadInfer m => m Bool
sprinkler = Sprinkler.soft

-- | Count the number of particles produced by SMC
checkParticles :: Int -> Int -> IO Int
checkParticles observations particles =
  newIOGenM (mkStdGen 1729)
    >>= sampleWith (fmap length (runPopulation $ smcMultinomial observations particles Sprinkler.soft))

checkParticlesSystematic :: Int -> Int -> IO Int
checkParticlesSystematic observations particles =
  newIOGenM (mkStdGen 1729)
    >>= sampleWith (fmap length (runPopulation $ smcSystematic observations particles Sprinkler.soft))

checkParticlesStratified :: Int -> Int -> IO Int
checkParticlesStratified observations particles =
  newIOGenM (mkStdGen 1729)
    >>= sampleWith (fmap length (runPopulation $ smcStratified observations particles Sprinkler.soft))

checkTerminateSMC :: IO [(Bool, Log Double)]
checkTerminateSMC =
  newIOGenM (mkStdGen 1729)
    >>= sampleWith (runPopulation $ smcMultinomial 2 5 sprinkler)

checkPreserveSMC :: Bool
checkPreserveSMC =
  (enumerate . collapse . smcMultinomial 2 2) sprinkler
    ~== enumerate sprinkler

expectationNearNumeric ::
  Weighted Integrator.Integrator Double ->
  Weighted Integrator.Integrator Double ->
  Double
expectationNearNumeric x y =
  let e1 = Integrator.expectation $ normalize x
      e2 = Integrator.expectation $ normalize y
   in (abs (e1 - e2))

expectationNearSampling ::
  Weighted (Sampler (IOGenM StdGen) IO) Double ->
  Weighted (Sampler (IOGenM StdGen) IO) Double ->
  IO Double
expectationNearSampling x y = do
  e1 <- sampleIOfixed $ fmap Sampler.sampleMean $ replicateM 10 $ Weighted.runWeighted x
  e2 <- sampleIOfixed $ fmap Sampler.sampleMean $ replicateM 10 $ Weighted.runWeighted y
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
