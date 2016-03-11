module TestDist where

import Data.AEq
import Data.Number.LogFloat

import Base
import Dist
import Sprinkler

unnorm :: MonadDist m => m Int
unnorm = discrete [0.5,0.8]

passed1 = fromLogFloat (evidence unnorm) ~== 1

agg :: MonadDist m => m Int
agg = do
  x <- uniformD [0,1]
  y <- uniformD [2,1]
  return (x+y)

passed2 = enumerate agg ~== [(1,0.25), (2,0.5), (3,0.25)]

passed3 = enumerate Sprinkler.hard ~== enumerate Sprinkler.soft
