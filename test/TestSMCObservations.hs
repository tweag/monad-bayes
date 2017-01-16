-- | Test whether a certain number of observations are sufficient
-- to generate samples of identical weight by SMC

module TestSMCObservations where

import Data.List

import Control.Monad.Bayes.LogDomain (LogDomain)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Sampler

smcParticles :: Int -> Int -> Sequential (Population SamplerIO) a -> IO [(a, LogDomain Double)]
smcParticles observations particles model = sampleIOfixed (runPopulation $ smc observations particles model)

sameWeights :: [(a, LogDomain Double)] -> Bool
sameWeights xs = length (nub $ map snd xs) == 1

-- | Check whether the weights of SMC particles are equal.
check_smc_weight :: Int -> Int -> Sequential (Population SamplerIO) a -> IO Bool
check_smc_weight observations particles model =
  do
    samples <- smcParticles observations particles model
    return $ length samples == particles && sameWeights samples
