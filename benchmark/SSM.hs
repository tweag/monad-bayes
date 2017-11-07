module Main where

import Control.Monad.IO.Class

import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference
import Control.Monad.Bayes.Inference.PMMH as PMMH

import NonlinearSSM

main :: IO ()
main = sampleIO $ do
  let t = 10
  dat <- generateData t
  let ys = map snd dat

  liftIO $ print "SMC"
  smcRes <- runPopulation $ smcMultinomial t 10 (model id ys)
  liftIO $ print $ show smcRes

  liftIO $ print "RM-SMC"
  smcrmRes <- runPopulation $ smcRM t 10 10 (model id ys)
  liftIO $ print $ show smcrmRes

  liftIO $ print "PMMH"
  pmmhRes <- pmmh 10 t 10 (model PMMH.latent ys)
  liftIO $ print $ show pmmhRes
