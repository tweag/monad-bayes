{-# LANGUAGE TypeFamilies #-}

module TestTraced where

import Control.Monad.Bayes.Class (uniformD, uniform)
import Control.Monad.Bayes.Sampler (sampleIOfixed)
import Control.Monad.Bayes.Traced (mh, mhWith, normalProposal)
import Control.Monad.Bayes.Weighted (prior)
import Data.List

uniformDWithMh :: IO [Int]
uniformDWithMh = do
  let sampler = prior . mh 10000 
  samples <- sampleIOfixed $ sampler $ uniformD [0, 1, 2]
  return $ take 9000 samples

uniformWithMhWithNormal :: IO [Double]
uniformWithMhWithNormal = do
  let sampler = prior . mhWith (normalProposal 0.1) 10000
  samples <- sampleIOfixed $ sampler $ uniform 0 2
  return $ take 9000 samples

check :: Real a => [a] -> Bool
check ls = abs (average ls - 1) < 0.01

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs
