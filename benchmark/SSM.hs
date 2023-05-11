module Main where

import Control.Monad.Bayes.Inference.MCMC
import Control.Monad.Bayes.Inference.PMMH as PMMH (pmmh)
import Control.Monad.Bayes.Inference.RMSMC (rmsmcDynamic)
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.SMC2 as SMC2 (smc2)
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Population (population, resampleMultinomial)
import Control.Monad.Bayes.Sampler.Strict (sampleIO, sampleIOfixed, sampleWith)
import Control.Monad.Bayes.Weighted (unweighted)
import Control.Monad.IO.Class (MonadIO (liftIO))
import NonlinearSSM (generateData, model, param)
import NonlinearSSM.Algorithms
import System.Random.Stateful (mkStdGen, newIOGenM)

main :: IO ()
main = sampleIOfixed $ do
  dat <- generateData t
  let ys = map snd dat
  liftIO $ print "SMC"
  smcRes <- runAlgFixed ys SMC
  liftIO $ print smcRes
  liftIO $ print "RM-SMC"
  smcrmRes <- runAlgFixed ys RMSMCDynamic
  liftIO $ print smcrmRes
  liftIO $ print "PMMH"
  pmmhRes <- runAlgFixed ys PMMH
  liftIO $ print pmmhRes
  liftIO $ print "SMC2"
  smc2Res <- runAlgFixed ys SMC2
  liftIO $ print $ show smc2Res
