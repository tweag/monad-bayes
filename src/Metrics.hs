module Metrics where

import Control.Arrow (second)
import Data.List (sort)
import Data.Maybe
import Data.Typeable

import Base
import Dist
import Primitive

-- | Kullback-Leibler divergence for discrete distributions.
--
-- Precondition: @p@ is absolutely continuous with respect to @q@.
-- Whenever @density_p x > 0@, so is @density_q x > 0@.
--
-- @
--    discreteKLDivergence p q = D_KL( p || q )
--                            = sum_x p(x) log (p(x) / q(x))
-- @
kullbackLeibnerDivergence :: (Ord a) => Dist a -> Dist a -> Double
kullbackLeibnerDivergence p q = sum $ do
  let qs = enumerate q
  let densityQ x = fromMaybe 0 $ lookup x qs
  (x, px) <- enumerate p
  return $ px * log (px / densityQ x)

-- | Measure goodness-of-fit of samples by Kullback-Leibner divergence
-- of the categorical distribution defined by the samples against the
-- target distribution.
kullbackLeibnerTest :: (Ord a, Typeable a) => [a] -> Dist a -> Double
kullbackLeibnerTest samples = kullbackLeibnerDivergence (categorical $ map (flip (,) 1) samples)

-- | The two-sample Kolmotorov-Smirnov distance between two sets of
-- samples is the maximum difference between their empirical distribution
-- functions.
kolmogorovSmirnovTest :: (Ord a) => [a] -> [a] -> Double
kolmogorovSmirnovTest xs ys =
  let
    zs = ksMerge 1 (sort xs) 1 (sort ys)
  in
    maximum $ map (\(_,cx,cy) -> abs (cx - cy)) zs
  where
    nxs = fromIntegral $ length xs
    nys = fromIntegral $ length ys
    ksMerge :: (Ord a) => Integer -> [a] -> Integer -> [a] -> [(a, Double, Double)]
    ksMerge cx [] cy [] = []
    ksMerge cx (x : xs) cy [] = (x, fromIntegral cx / nxs, fromIntegral cy / nys) : ksMerge (cx + 1) xs cy []
    ksMerge cx [] cy (y : ys) = (y, fromIntegral cx / nxs, fromIntegral cy / nys) : ksMerge cx [] (cy + 1) ys
    ksMerge cx (x : xs) cy (y : ys) =
      if x <= y then
        (x, fromIntegral cx / nxs, fromIntegral cy / nys) : ksMerge (cx + 1) xs cy (y : ys)
      else
        (y, fromIntegral cx / nxs, fromIntegral cy / nys) : ksMerge cx (x : xs) (cy + 1) ys

