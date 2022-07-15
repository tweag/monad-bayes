module TestAdvanced where

import ConjugatePriors
  ( betaBernoulli',
    betaBernoulliAnalytic,
    gammaNormal',
    gammaNormalAnalytic,
    normalNormal',
    normalNormalAnalytic,
  )
import Control.Arrow
import Control.Monad (join, replicateM)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Inference.MCMC
import Control.Monad.Bayes.Inference.PMMH
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.SMC2
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted

passed1 = do
  sample <- sampleIOfixed $ unweighted $ mh 10000 random
  return $ abs (0.5 - (expectation id $ fromList $ toEmpirical sample)) < 0.01

passed2 = do
  sample <- sampleIOfixed $ population $ smc (SMCConfig {numSteps = 0, numParticles = 10000, resampler = resampleMultinomial}) random
  return $ close 0.5 sample

mcmcConfig = MCMCConfig {numMCMCSteps = 0, numBurnIn = 0, proposal = SingleSiteMH}

smcConfig = SMCConfig {numSteps = 0, numParticles = 1000}

passed3 = do
  sample <- sampleIOfixed $ population $ rmsmcDynamic mcmcConfig smcConfig random
  return $ close 0.5 sample

passed4 = do
  sample <- sampleIOfixed $ population $ rmsmcBasic mcmcConfig smcConfig random
  return $ close 0.5 sample

passed5 = do
  sample <- sampleIOfixed $ population $ rmsmc mcmcConfig smcConfig random
  return $ close 0.5 sample

passed6 = do
  sample <- fmap join $ sampleIOfixed $ unweighted $ pmmh 100 0 100 random (normal 0)
  return $ close 0.0 sample

passed7 = do
  sample <- fmap (join . take 50) $ sampleIOfixed $ fmap (fmap (\(x, y) -> fmap (second (* y)) x)) $ population $ smc2 0 100 100 100 random (normal 0)
  return $ close 0.0 sample

close n sample = abs (n - (expectation id $ fromList $ toEmpiricalWeighted sample)) < 0.01
