{-# LANGUAGE
  TupleSections
 #-}

module Examples where

import System.Random

import Base
import Dist
import Sampler
import Inference

g = mkStdGen 0

die :: MonadDist m => m Int
die = categorical $ map (,1/6) [1..6]

dice :: MonadBayes m => m Int
dice = do
  x <- die
  factor (1 / fromIntegral x)
  y <- die
  factor (1 / fromIntegral y)
  return (x+y)

exact = enumerate dice

imp = sample (importance' 1000 dice) g

sequential = sample (smc' 1000 dice) g
