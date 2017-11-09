module Main where

import Control.Monad.IO.Class

import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Inference.PMMH as PMMH
import Control.Monad.Bayes.Inference.SMC2 as SMC2

import NonlinearSSM

main :: IO ()
main = sampleIO $ do
  let t = 5
  dat <- generateData t
  let ys = map snd dat

  liftIO $ print "SMC"
  smcRes <- runPopulation $ smcMultinomial t 10 (model id ys)
  liftIO $ print $ show smcRes

  liftIO $ print "RM-SMC"
  smcrmRes <- runPopulation $ rmsmc t 10 10 (model id ys)
  liftIO $ print $ show smcrmRes

  liftIO $ print "PMMH"
  pmmhRes <- prior $ pmmh 2 t 3 (model PMMH.latent ys)
  liftIO $ print $ show pmmhRes

  liftIO $ print "SMC2"
  smc2Res <- runPopulation $ smc2 t 3 2 1 (model SMC2.latent ys)
  liftIO $ print $ show smc2Res
