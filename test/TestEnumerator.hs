{-# LANGUAGE ImportQualifiedPost #-}

module TestEnumerator (passed1, passed2, passed3, passed4) where

import Control.Monad.Bayes.Class
  ( MonadSample (categorical, uniformD),
  )
import Control.Monad.Bayes.Enumerator
  ( enumerated,
    evidence,
    expectation,
  )
import Data.AEq (AEq ((~==)))
import Data.Vector qualified as V
import Numeric.Log (Log (ln))
import Sprinkler (hard, soft)

unnorm :: MonadSample m => m Int
unnorm = categorical $ V.fromList [0.5, 0.8]

passed1 :: Bool
passed1 = (exp . ln) (evidence unnorm) ~== 1

agg :: MonadSample m => m Int
agg = do
  x <- uniformD [0, 1]
  y <- uniformD [2, 1]
  return (x + y)

passed2 :: Bool
passed2 = enumerated agg ~== [(2, 0.5), (1, 0.25), (3, 0.25)]

passed3 :: Bool
passed3 = enumerated Sprinkler.hard ~== enumerated Sprinkler.soft

passed4 :: Bool
passed4 =
  expectation (^ (2 :: Int)) (fmap (fromIntegral . (+ 1)) $ categorical $ V.fromList [0.5, 0.5]) ~== 2.5
