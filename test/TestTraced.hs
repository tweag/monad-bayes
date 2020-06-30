{-# LANGUAGE TypeFamilies #-}

module TestTraced where

import Control.Monad.Bayes.Class (normal, uniform, uniformD)
import Control.Monad.Bayes.Sampler (sampleIOfixed)
import Control.Monad.Bayes.Traced (mh, mhWith, normalProposal)
import Control.Monad.Bayes.Weighted (prior)
import Data.List

uniformDWithMh :: IO [Int]
uniformDWithMh = do
  let sampler = prior . mh 10000
  samples <- sampleIOfixed $ sampler $ uniformD [0, 1, 2]
  return $ take 9000 samples

checkUniformDWithMh :: [Int] -> Bool
checkUniformDWithMh xs = approx (average xs) 1.0

uniformWithMhWithNormal :: IO [Double]
uniformWithMhWithNormal = do
  let sampler = prior . mhWith (normalProposal 0.1) 10000
  samples <- sampleIOfixed $ sampler $ uniform 0 2
  return $ take 9000 samples

checkUniformWithMhWithNormal :: [Double] -> Bool
checkUniformWithMhWithNormal xs = approx (average xs) 1.0

normalWithMhWithNormal :: IO [Double]
normalWithMhWithNormal = do
  let sampler = prior . mhWith (normalProposal 0.1) 50000
  samples <- sampleIOfixed $ sampler $ normal 3.0 2.0
  return $ take 50000 samples

checkNormalWithMhWithNormal :: [Double] -> Bool
checkNormalWithMhWithNormal xs = approx (average xs) 3.0 && approx (std xs) 2.0

approx :: (Real a, Fractional a) => a -> a -> Bool
approx val1 val2 = abs (val1 - val2) < 0.01

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

var :: [Double] -> Double
var xs = (sum sqDeviations) / genericLength xs
  where
    av = average xs
    sqDeviations = fmap (\e -> (e - av)**2) xs

std :: [Double] -> Double
std xs = sqrt $ var xs
