

module Dice where

-- A toy model for dice rolling from http://dl.acm.org/citation.cfm?id=2804317

import Control.Monad (liftM2)

import Base
import Dist

die :: (Monad d, UniformD n d, Integral n) => n -> d n
-- | A distribution over the sums of results of n independent rolls of a fair die
die 0 = return 0
die 1 = uniformd [1,2,3,4,5,6]
die n = liftM2 (+) (die 1) (die (n-1))

conditionalDie n = condition (\n -> 1 / fromIntegral n) (die n)
