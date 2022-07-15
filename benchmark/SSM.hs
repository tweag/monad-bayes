module Main where

import Control.Monad.Bayes.Inference.PMMH as PMMH (pmmh)
import Control.Monad.Bayes.Inference.RMSMC (rmsmcDynamic)
import Control.Monad.Bayes.Inference.SMC (SMCConfig (SMCConfig, numParticles, numSteps, resampler), smc)
import Control.Monad.Bayes.Inference.SMC2 as SMC2 (smc2)
import Control.Monad.Bayes.Population (population, resampleMultinomial)
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Bayes.Weighted (unweighted)
import Control.Monad.IO.Class (MonadIO (liftIO))
import NonlinearSSM (generateData, model, param)

main :: IO ()
main = sampleIO $ do
  let t = 5
  dat <- generateData t
  let ys = map snd dat
  liftIO $ print "SMC"
  smcRes <- population $ smc SMCConfig {numSteps = t, numParticles = 10, resampler = resampleMultinomial} (param >>= model ys)
  liftIO $ print $ show smcRes
  liftIO $ print "RM-SMC"
  smcrmRes <- population $ rmsmcDynamic t 10 10 (param >>= model ys)
  liftIO $ print $ show smcrmRes
  liftIO $ print "PMMH"
  pmmhRes <- unweighted $ pmmh 2 t 3 param (model ys)
  liftIO $ print $ show pmmhRes
  liftIO $ print "SMC2"
  smc2Res <- population $ smc2 t 3 2 1 param (model ys)
  liftIO $ print $ show smc2Res
