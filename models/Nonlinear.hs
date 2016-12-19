{-# LANGUAGE
 FlexibleContexts,
 TypeFamilies
 #-}

module Nonlinear (
  model
  ) where

import Control.Monad.Bayes.Class

model :: MonadDist m => Int -> m [(CustomReal m, CustomReal m)]
model t = do
  let simulate 0 _ acc = return acc
      simulate n x acc = do
        x' <- normal (0.5 * x + 25 * x / (1 + x^2) + 8 * (cos (1.2 * fromIntegral n))) 1
        y' <- normal (x' ^ 2 / 20) 1
        simulate (n-1) x' ((x',y'):acc)

  x0 <- normal 0 (sqrt 5)
  ps <- simulate t x0 []
  return $ reverse ps
