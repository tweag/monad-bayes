-- Branching model from Anglican (https://bitbucket.org/probprog/anglican-white-paper)

module Branching where

import Control.Monad.Bayes.Simple

fib :: Int -> Int
fib n = run 1 1 0 where
  run a b m = if n == m then a else run b (a + b) (m+1)

branching :: (MonadBayes m, CustomReal m ~ Double) => m Int
branching = do
  let count_prior = discrete $ replicate 10 0.1 -- TODO: change to poisson 4
  r <- count_prior
  l <- if (4 < r) then
         return 6
       else
         fmap (+ fib (3*r)) count_prior
  observe (discreteDist $ replicate 10 0.1) 6 -- TODO: change to poisson l
  return r
