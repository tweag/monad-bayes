{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Trustworthy #-}

module TestDistribution
  ( passed1,
    passed2,
    passed3,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Bayes.Class (MonadSample, mvNormal)
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (evalStateT)
import Data.Matrix (fromList)
import Data.Vector qualified as V
import System.Random.MWC (toSeed)

-- Test the sampled covariance is approximately the same as the
-- specified covariance.
passed1 :: IO Bool
passed1 = sampleIOfixed $ do
  let mu = (V.fromList [0.0, 0.0])
      sigma11 = 2.0
      sigma12 = 1.0
      bigSigma = (fromList 2 2 [sigma11, sigma12, sigma12, sigma11])
      nSamples = 200000
      nSamples' = fromIntegral nSamples
  ss <- replicateM nSamples $ (mvNormal mu bigSigma)
  let xbar = (/ nSamples') $ sum $ fmap (V.! 0) ss
      ybar = (/ nSamples') $ sum $ fmap (V.! 1) ss
  let term1 = (/ nSamples') $ sum $ zipWith (*) (fmap (V.! 0) ss) (fmap (V.! 1) ss)
  let term2 = xbar * ybar
  return $ abs (sigma12 - (term1 - term2)) < 2e-2

-- Test the sampled means are approximately the same as the specified
-- means.
passed2 :: IO Bool
passed2 = sampleIOfixed $ do
  let mu = (V.fromList [0.0, 0.0])
      sigma11 = 2.0
      sigma12 = 1.0
      bigSigma = (fromList 2 2 [sigma11, sigma12, sigma12, sigma11])
      nSamples = 100000
      nSamples' = fromIntegral nSamples
  ss <- replicateM nSamples $ (mvNormal mu bigSigma)
  let xbar = (/ nSamples') $ sum $ fmap (V.! 0) ss
      ybar = (/ nSamples') $ sum $ fmap (V.! 1) ss
  return $ abs xbar < 1e-2 && abs ybar < 1e-2

-- Test the sampled variances are approximately the same as the
-- specified variances.
passed3 :: IO Bool
passed3 = sampleIOfixed $ do
  let mu = (V.fromList [0.0, 0.0])
      sigma11 = 2.0
      sigma12 = 1.0
      bigSigma = (fromList 2 2 [sigma11, sigma12, sigma12, sigma11])
      nSamples = 200000
      nSamples' = fromIntegral nSamples
  ss <- replicateM nSamples $ (mvNormal mu bigSigma)
  let xbar = (/ nSamples') $ sum $ fmap (V.! 0) ss
      ybar = (/ nSamples') $ sum $ fmap (V.! 1) ss
  let xbar2 = (/ nSamples') $ sum $ fmap (\x -> x * x) $ fmap (V.! 0) ss
      ybar2 = (/ nSamples') $ sum $ fmap (\x -> x * x) $ fmap (V.! 1) ss
  let xvar = xbar2 - xbar * xbar
  let yvar = ybar2 - ybar * ybar
  return $ abs (xvar - sigma11) < 1e-2 && abs (yvar - sigma11) < 2e-2
