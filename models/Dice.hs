

module Dice where

-- A toy model for dice rolling from http://dl.acm.org/citation.cfm?id=2804317
-- Exact results can be obtained using Dist monad

import Control.Monad (liftM2)
import Control.Monad.Bayes.Class

-- | A toss of a six-sided die.
die :: MonadSample m => m Int
die = uniformD [1..6]

-- | A sum of outcomes of n independent tosses of six-sided dice.
dice :: MonadSample m => Int -> m Int
dice 1 = die
dice n = liftM2 (+) die (dice (n-1))

-- | Toss of two dice where the output is greater than 4.
dice_hard :: MonadInfer m => m Int
dice_hard = do
  result <- dice 2
  condition (result > 4)
  return result

-- | Toss of two dice with an artificial soft constraint.
dice_soft :: MonadInfer m => m Int
dice_soft = do
  result <- dice 2
  score (1 / fromIntegral result)
  return result
