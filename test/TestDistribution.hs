{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Trustworthy #-}

module TestDistribution (passed1) where

import Control.Monad (replicateM)
import Control.Monad.Bayes.Class (mvNormal)
import Control.Monad.Bayes.Sampler
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (evalStateT)
import Data.Matrix (fromList)
import Data.Vector qualified as V
import System.Random.MWC (toSeed)

passed1 :: Bool
passed1 = runIdentity $ evalStateT r (toSeed (V.fromList [42, 1729]))
  where
    r = do
      let mu = (V.fromList [0.0, 0.0])
          sigma11 = 2.0
          sigma12 = 1.0
          bigSigma = (fromList 2 2 [sigma11, sigma12, sigma12, sigma11])
          nSamples = 100000
          nSamples' = fromIntegral nSamples
      ss <- sampleST $ replicateM nSamples $ (mvNormal mu bigSigma)
      let xbar = (/ nSamples') $ sum $ fmap (V.! 0) ss
          ybar = (/ nSamples') $ sum $ fmap (V.! 1) ss
      let term1 = (/ nSamples') $ sum $ zipWith (*) (fmap (V.! 0) ss) (fmap (V.! 1) ss)
      let term2 = xbar * ybar
      return $ abs (sigma12 - (term1 - term2)) < 1e-2
