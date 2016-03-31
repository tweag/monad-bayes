-- | Test whether a certain number of observations are sufficient
-- to generate samples of identical weight by SMC

module TestSMCObservations where

import Data.List
import Data.Number.LogFloat
import System.Random

import Base
import Empirical
import Inference
import Particle
import Sampler

import qualified HMM as HMM

g = mkStdGen 0

smcParticles :: Int -> Int -> ParticleT (EmpiricalT Sampler) a -> [(a, LogFloat)]
smcParticles observations particles model = sample (runEmpiricalT $ smc observations particles model) g

sameWeights :: [(a, LogFloat)] -> Bool
sameWeights xs = length (nub $ map snd xs) == 1

-- | Check whether the weights of SMC particles are equal.
check_smc_weight :: Int -> Int -> ParticleT (EmpiricalT Sampler) a -> Bool
check_smc_weight observations particles model =
  let
    samples = smcParticles observations particles model
  in
    length samples == particles && sameWeights samples
