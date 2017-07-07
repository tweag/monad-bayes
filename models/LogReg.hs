-- Logistic regression model from Anglican
-- (https://bitbucket.org/probprog/anglican-white-paper)

module LogReg where

import Numeric.LogDomain
import Control.Monad.Bayes.Simple

import Debug.Trace (traceM, traceShowM)

xs :: [Double]
xs = [-10, -5, 2, 6, 10]

labels :: [Bool]
labels = [False, False, True, True, True]

logisticRegression :: (MonadBayes m, CustomReal m ~ Double) => m Double
logisticRegression = do
  m <- normal 0 1
  b <- normal 0 1
  sigma <- gamma 1 1
  let y x = normal (m * x + b) sigma
      sigmoid x = y x >>= \t -> return $ 1 / (1 + exp (- t))
      obs x label = do
        p <- sigmoid x
        factor $ toLogDomain $ if label then p else 1 - p
  mapM_ (uncurry obs) (zip xs labels)
  sigmoid 8
