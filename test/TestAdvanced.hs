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
  print $ expectation id $ fromList $ toEmpirical sample

passed2 = do
  sample <- sampleIOfixed $ population $ smcMultinomial 0 10000 random
  print $ expectation id $ fromList $ toEmpiricalWeighted sample

passed3 = do
  sample <- sampleIOfixed $ population $ rmsmcLocal 0 1000 0 random
  print $ expectation id $ fromList $ toEmpiricalWeighted sample

passed4 = do
  sample <- sampleIOfixed $ population $ rmsmcBasic 0 1000 0 random
  print $ expectation id $ fromList $ toEmpiricalWeighted sample

passed5 = do
  sample <- sampleIOfixed $ population $ rmsmc 0 1000 0 random
  print $ expectation id $ fromList $ toEmpiricalWeighted sample

passed6 = do
  sample <- fmap join $ sampleIOfixed $ unweighted $ pmmh 100 0 100 random (normal 0)
  print $ expectation id $ fromList $ toEmpiricalWeighted sample

passed7 = do
  sample <- fmap join $ sampleIO $ fmap (fmap (\(x, y) -> fmap (second (* y)) x)) $ population $ smc2 0 100 100 100 random (normal 0)
  print $ expectation id $ fromList $ toEmpiricalWeighted sample
