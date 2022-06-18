module Main where

import Control.Monad.Bayes.Inference.PMMH as PMMH
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.SMC2 as SMC2
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.IO.Class
import NonlinearSSM
import System.Random.Stateful

main :: IO ()
main = do
  (smcRes, smcrmRes, pmmhRes, smc2Res) <-
    newIOGenM (mkStdGen 1729) >>=
    (sampleIOwith $ do
        let t = 5
        dat <- generateData t
        let ys = map snd dat
        smcRes <- runPopulation $ smcMultinomial t 10 (param >>= model ys)
        smcrmRes <- runPopulation $ rmsmcLocal t 10 10 (param >>= model ys)
        pmmhRes <- prior $ pmmh 2 t 3 param (model ys)
        smc2Res <- runPopulation $ smc2 t 3 2 1 param (model ys)
        return (smcRes, smcrmRes, pmmhRes, smc2Res))
  print "SMC"
  print $ show smcRes
  print "RM-SMC"
  print $ show smcrmRes
  print "PMMH"
  print $ show pmmhRes
  print "SMC2"
  print $ show smc2Res
