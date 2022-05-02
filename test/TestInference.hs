{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module TestInference where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced.Static
import Data.AEq
import Numeric.Log
import Sprinkler
import ConjugatePriors
import qualified Control.Monad.Bayes.Integrator as Integrator
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Bayes.Traced.Static (estimateMeanVarianceMH)

sprinkler :: MonadInfer m => m Bool
sprinkler = Sprinkler.soft

-- | Count the number of particles produced by SMC
checkParticles :: Int -> Int -> IO Int
checkParticles observations particles =
  sampleIOfixed (fmap length (runPopulation $ smcMultinomial observations particles Sprinkler.soft))

checkParticlesSystematic :: Int -> Int -> IO Int
checkParticlesSystematic observations particles =
  sampleIOfixed (fmap length (runPopulation $ smcSystematic observations particles Sprinkler.soft))

checkTerminateSMC :: IO [(Bool, Log Double)]
checkTerminateSMC = sampleIOfixed (runPopulation $ smcMultinomial 2 5 sprinkler)

checkPreserveSMC :: Bool
checkPreserveSMC =
  (enumerate . collapse . smcMultinomial 2 2) sprinkler
    ~== enumerate sprinkler

expectationNear x y = do
    (e1) <- estimateMeanEmpirical x
    (e2) <- estimateMeanEmpirical y
    return (abs (e1 - e2))

-- expectationNearAnalytic :: Monad m =>
--   Weighted Integrator.Integrator Double
--   -> Weighted Integrator.Integrator Double -> m Double
expectationNearAnalytic x y = do
    let e1 = Integrator.expectation $ Integrator.normalize x
        e2 = Integrator.expectation $ Integrator.normalize y
    return (abs (e1 - e2))

testNormalNormal :: [Double] -> IO Bool
testNormalNormal n = do

  e <- expectationNear
    (posterior (normalNormal' 1 (1,1)) n)
    (normalNormalAnalytic 1 (1,1) n)

  return (e < 1e-0)

testGammaNormal :: [Double] -> IO Bool
testGammaNormal n = do

  e <- expectationNearAnalytic
    (posterior (gammaNormal' (1,1)) n)
    (gammaNormalAnalytic (1,1) n)
  return (e < 1e-1)

testBetaBernoulli :: [Bool] -> IO Bool
testBetaBernoulli bs = do

  e <- expectationNearAnalytic
    (posterior (betaBernoulli' (1,1)) bs)
    (betaBernoulliAnalytic (1,1) bs)
  
  return (e < 1e-1)