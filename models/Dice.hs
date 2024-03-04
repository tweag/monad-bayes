module Dice (diceHard, diceSoft) where

-- A toy model for dice rolling from http://dl.acm.org/citation.cfm?id=2804317
-- Exact results can be obtained using Dist monad

import Control.Applicative (Applicative (..))
import Control.Monad.Bayes.Class
  ( MonadDistribution (uniformD),
    MonadFactor (score),
    MonadMeasure,
    condition,
  )
-- Prelude exports liftA2 from GHC 9.6 on, see https://github.com/haskell/core-libraries-committee/blob/main/guides/export-lifta2-prelude.md
-- import Control.Applicative further up can be removed once we don't support GHC <= 9.4 anymore

import Prelude hiding (Applicative (..))

-- | A toss of a six-sided die.
die :: (MonadDistribution m) => m Int
die = uniformD [1 .. 6]

-- | A sum of outcomes of n independent tosses of six-sided dice.
dice :: (MonadDistribution m) => Int -> m Int
dice 1 = die
dice n = liftA2 (+) die (dice (n - 1))

-- | Toss of two dice where the output is greater than 4.
diceHard :: (MonadMeasure m) => m Int
diceHard = do
  result <- dice 2
  condition (result > 4)
  return result

-- | Toss of two dice with an artificial soft constraint.
diceSoft :: (MonadMeasure m) => m Int
diceSoft = do
  result <- dice 2
  score (1 / fromIntegral result)
  return result
