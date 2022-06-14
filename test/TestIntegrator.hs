{-# LANGUAGE BlockArguments #-}

module TestIntegrator where

import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
  ( MonadCond (score),
    MonadInfer,
    MonadSample (bernoulli, gamma, normal, random, uniformD),
    condition,
    factor,
    normalPdf,
  )
import Control.Monad.Bayes.Integrator
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted (runWeighted)
import Data.AEq (AEq ((~==)))
import Data.List (sortOn)
import Data.Set (fromList)
import Numeric.Log (Log (Exp, ln))
import Sprinkler (hard, soft)
import Statistics.Distribution (Distribution (cumulative))
import Statistics.Distribution.Normal (normalDistr)

-- (sampleMean)

normalExpectation :: Double -> Double -> Double
normalExpectation mean std = expectation (normal mean std)

normalVariance :: Double -> Double -> Double
normalVariance mean std = variance (normal mean std)

volumeIsOne :: [Double] -> Bool
volumeIsOne = (~== 1.0) . volume . uniformD

agg :: MonadSample m => m Int
agg = do
  x <- uniformD [0, 1]
  y <- uniformD [2, 1]
  return (x + y)

passed1,
  passed2,
  passed3,
  passed4,
  passed5,
  passed6,
  passed7,
  passed8,
  passed9,
  passed10,
  passed11,
  passed12,
  passed13,
  passed14 ::
    Bool
passed1 =
  sortOn fst (enumerateWith (fromList [3, 1, 2]) agg)
    ~== sortOn fst [(2, 0.5), (1, 0.25), (3, 0.25)]
passed2 =
  enumerateWith (fromList [True, False]) (normalize (Sprinkler.hard))
    ~== enumerateWith (fromList [True, False]) (normalize (Sprinkler.soft))
passed3 =
  expectation (fmap ((** 2) . (+ 1)) $ uniformD [0, 1]) == 2.5
passed4 = volume (uniformD [1, 2]) ~== 1.0
passed5 =
  sortOn fst (enumerateWith (fromList [0, 1 :: Int]) (empirical [0 :: Int, 1, 1, 1]))
    == sortOn fst [(1, 0.75), (0, 0.25)]
passed6 =
  sortOn fst [(2, 0.5), (3, 0.5), (1, 0.0)]
    == sortOn
      fst
      ( enumerateWith (fromList [1, 2, 3]) $
          normalize $ do
            x <- uniformD [1 :: Int, 2, 3]
            condition (x > 1)
            return x
      )
passed7 =
  sortOn fst [(True, 0.75), (False, 0.25)]
    ~== sortOn
      fst
      ( enumerateWith (fromList [True, False]) $ normalize do
          x <- bernoulli 0.5
          factor $ if x then 0.3 else 0.1
          return x
      )
passed8 =
  1
    == ( volume $
           fmap (ln . exp . snd) $ runWeighted do
             x <- bernoulli 0.5
             factor $ if x then 0.2 else 0.1
             return x
       )
passed9 = probability (1, 1000) (normal 1 10) - 0.5 < 0.05
passed10 = cdf (normal 5 5) 5 - 0.5 < 0.05
passed11 =
  (within 0.001)
    ( cdf
        ( do
            x <- normal 0 1
            return x
        )
        3
    )
    (cumulative (normalDistr 0 1) 3)

within :: (Ord a, Num a) => a -> a -> a -> Bool
within n x y = abs (x - y) < n

passed12 =
  volume
    ( do
        x <- gamma 2 3
        return x
    )
    ~== 1

passed13 =
  (volume . normalize)
    ( do
        x <- gamma 2 3
        factor (normalPdf 0 1 x)
        return x
    )
    ~== 1

passed14 =
  let sample = sampleSTfixed $ fmap sampleMean $ replicateM 10000 $ runWeighted $ model1
      quadrature = expectation $ normalize $ model1
   in abs (sample - quadrature) < 0.01

model1 :: MonadInfer m => m Double
model1 = do
  x <- random
  y <- random
  -- score 0.5
  score (Exp $ log (f x + y))
  return x
  where
    f x = cos (x ** 4) + x ** 3
