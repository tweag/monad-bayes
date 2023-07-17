module Main where

import Control.Monad (forM_)
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
  forM_ [SMC, RMSMCDynamic, PMMH, SMC2] $ \alg -> do
    liftIO $ print alg
    result <- runAlgFixed ys alg
    liftIO $ putStrLn result
