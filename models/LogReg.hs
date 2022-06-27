
-- Logistic regression model from Anglican
-- (https://bitbucket.org/probprog/anglican-white-paper)

module LogReg (logisticRegression, syntheticData, xs, labels) where

import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
  ( MonadInfer,
    MonadSample (bernoulli, gamma, normal, uniform),
    factor,
  )
import Numeric.Log (Log (Exp))

logisticRegression :: MonadInfer Double m => [(Double, Bool)] -> m Double
logisticRegression dat = do
  m <- normal 0 1
  b <- normal 0 1
  sigma <- gamma 1 1
  let y x = normal (m * x + b) sigma
      sigmoid x = y x >>= \t -> return $ 1 / (1 + exp (-t))
      obs x label = do
        p <- sigmoid x
        factor $ (Exp . log) $ if label then p else 1 - p
  mapM_ (uncurry obs) dat
  sigmoid 8

-- make a synthetic dataset by randomly choosing input-label pairs
syntheticData :: MonadSample Double m => Int -> m [(Double, Bool)]
syntheticData n = replicateM n do
  x <- uniform (-1) 1
  label <- bernoulli 0.5
  return (x, label)

-- a tiny test dataset, for sanity-checking
xs :: [Double]
xs = [-10, -5, 2, 6, 10]

labels :: [Bool]
labels = [False, False, True, True, True]
