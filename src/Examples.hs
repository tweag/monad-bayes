{-# LANGUAGE
  TupleSections
 #-}

module Examples where

import System.Random

import Base
import Dist

g = mkStdGen 0

die :: MonadDist m => m Int
die = categorical $ map (,1/6) [1..6]

dice :: MonadDist m => m Int
dice = do
  x <- die
  y <- die
  return (x+y)
