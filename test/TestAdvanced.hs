module TestAdvanced where

import Control.Arrow
import Control.Monad (join)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Inference.MCMC
import Control.Monad.Bayes.Inference.PMMH
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.SMC2
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler.Strict

mcmcConfig :: MCMCConfig
mcmcConfig = MCMCConfig {numMCMCSteps = 0, numBurnIn = 0, proposal = SingleSiteMH}

smcConfig :: (MonadDistribution m) => SMCConfig m
smcConfig = SMCConfig {numSteps = 0, numParticles = 1000, resampler = resampleMultinomial}

passed1, passed2, passed3, passed4, passed5, passed6, passed7 :: IO Bool
passed1 = do
  sample <- sampleIOfixed $ mcmc MCMCConfig {numMCMCSteps = 10000, numBurnIn = 5000, proposal = SingleSiteMH} random
  return $ abs (0.5 - (expectation id $ fromList $ toEmpirical sample)) < 0.01
passed2 = do
  sample <- sampleIOfixed $ population $ smc (SMCConfig {numSteps = 0, numParticles = 10000, resampler = resampleMultinomial}) random
  return $ close 0.5 sample
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
  sample <-
    fmap join $
      sampleIOfixed $
        pmmh
          mcmcConfig {numMCMCSteps = 100}
          smcConfig {numSteps = 0, numParticles = 100}
          random
          (normal 0)
  return $ close 0.0 sample

close :: Double -> [(Double, Log Double)] -> Bool

passed7 = do
  sample <- fmap join $ sampleIOfixed $ fmap (fmap (\(x, y) -> fmap (second (* y)) x)) $ population $ smc2 0 100 100 100 random (normal 0)
  return $ close 0.0 sample

close n sample = abs (n - (expectation id $ fromList $ toEmpiricalWeighted sample)) < 0.01
