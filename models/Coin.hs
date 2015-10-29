
module Coin where

-- Coin tossing with Beta prior

import Data.List (partition)

import Base
import Dist
import Explicit

coinToss :: (Functor d, Monad d, Conditional d, Beta d) => Double -> Double -> [Bool] -> d Prob
-- | A biased coin with a finite number of observed tosses.
-- The prior on the coin weight is 'beta' a b.
coinToss a b results =
    register prior results where
        prior = fmap prob $ beta a b
        register d [] = d
        register d (r:rs) = register (observe d (bernoulli :: Prob -> Explicit Bool) r) rs

coinExact :: (Functor d, Beta d) => Double -> Double -> [Bool] -> d Prob
-- | Exact posterior for the 'coinToss' example.
coinExact a b results =
    fmap prob $ beta (a + positive) (b + negative) where
        (p,n) = partition id results
        positive = fromIntegral $ length p
        negative = fromIntegral $ length n
