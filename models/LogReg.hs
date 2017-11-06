-- Logistic regression model from Anglican
-- (https://bitbucket.org/probprog/anglican-white-paper)

module LogReg where

import Numeric.Log
import Control.Monad.Bayes.Class

xs :: [Double]
xs = [-10, -5, 2, 6, 10]

labels :: [Bool]
labels = [False, False, True, True, True]

logisticRegression :: (MonadInfer m) => m Double
logisticRegression = do
  m <- normal 0 1
  b <- normal 0 1
  sigma <- gamma 1 1
  let y x = normal (m * x + b) sigma
      sigmoid x = y x >>= \t -> return $ 1 / (1 + exp (- t))
      obs x label = do
        p <- sigmoid x
        factor $ (Exp . log) $ if label then p else 1 - p
  mapM_ (uncurry obs) (zip xs labels)
  sigmoid 8
