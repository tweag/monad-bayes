
module KS where

import Base

ks :: (Ord a, Cumulative d', Sampleable d) => StdGen -> Int -> d a -> d' a -> Double
ks g n d d' =
