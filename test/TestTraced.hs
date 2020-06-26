{-# LANGUAGE TypeFamilies #-}

module TestTraced where

import Control.Monad.Bayes.Class (uniformD)
import Control.Monad.Bayes.Sampler (sampleIOfixed)
import Control.Monad.Bayes.Traced (mh)
import Control.Monad.Bayes.Weighted (prior)

result :: IO [Int]
result = do
  samples <- sampleIOfixed $ prior $ mh 10000 $ uniformD [0, 1, 2]
  return $ take 9000 samples

check :: [Int] -> Bool
check ls = abs (mean ls - 1) < 0.01

mean :: [Int] -> Double
mean ls = fromIntegral (Prelude.sum ls) / fromIntegral (length ls)
