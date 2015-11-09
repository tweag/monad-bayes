
module KL where

import Base
import Explicit


kl :: (Eq a, Density d) => Explicit a -> d a -> Prob
-- | Computes the Kullback-Leibler divergence between two distributions.
-- Generality comes with a quadratic time cost.
kl p q =
  sum $ map f xs where
    xs = toList $ compact p
    f (x,w) = case density q x of
      0 -> error "Undefined KL divergence - q is 0 when p is not"
      a -> w * log (w / a)

fastKL :: (Ord a, Density d) => Explicit a -> d a -> Prob
-- | Faster version of 'kl' for ordered types.
-- Internally uses a 'fastCompact' instead of 'compact'.
fastKL p q =
  sum $ map f xs where
    xs = toList $ fastCompact p
    f (x,w) = case density q x of
      0 -> error "Undefined KL divergence - q is 0 when p is not"
      a -> w * log (w / a)

