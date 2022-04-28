{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module TestInference where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler
import Data.AEq
import Numeric.Log
import Sprinkler
import ConjugatePriors

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
    (e1, var1) <- estimateMeanVarianceMH x
    (e2, var2) <- estimateMeanVarianceMH y
    return (abs (e1 - e2), abs (var1 - var2))

testNormalNormal :: [Double] -> IO Bool
testNormalNormal n = do

  (e,_) <- expectationNear
    (posterior (normalNormal' 1 (1,1)) [1.0])
    (normalNormalAnalytic 1 (1,1) [1.0])

  return (e < 1e-1)

testGammaNormal :: [Double] -> IO Bool
testGammaNormal n = do

  (e, _) <- expectationNear
    (posterior (gammaNormal' (1,1)) n)
    (gammaNormalAnalytic (1,1) n)
  return (e < 1e-1)

testBetaBernoulli :: [Double] -> IO Bool
testBetaBernoulli n = do

  (e,_) <- expectationNear
    (posterior (betaBernoulli' (1,1)) [True])
    (betaBernoulliAnalytic (1,1) [1])
  
  return (e < 1e-1)