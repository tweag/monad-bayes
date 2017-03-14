-- A model in which a random value switches between
-- two distributions, one with a support strictly
-- smaller than the other.
module StrictlySmallerSupport where

import Control.Monad.Bayes.Simple

model :: MonadDist m => m Bool
model = do
  x <- bernoulli 0.5
  y <- uniformD (if x then [1, 2] else [1, 2, 3, 4] :: [Int])
  return x
