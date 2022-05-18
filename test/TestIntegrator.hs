module TestIntegrator where

import Control.Monad.Bayes.Class
    ( MonadSample(normal, uniformD, bernoulli), condition, factor )
import Control.Monad.Bayes.Weighted (runWeighted)
import Control.Monad.Bayes.Integrator
    ( cdf,
      empirical,
      enumerateWith,
      expectation,
      probability,
      variance,
      volume,
      normalize )
import Data.AEq ( AEq((~==)) )
import Numeric.Log ( Log(ln) )
import Sprinkler ( hard, soft )
import Data.Set (fromList)

normalExpectation :: Double -> Double -> Double
normalExpectation mean std = expectation (normal mean std)

normalVariance :: Double -> Double -> Double
normalVariance mean std = variance (normal mean std)

volumeIsOne :: [Double] -> Bool
volumeIsOne = (~==1.0) . volume . uniformD

agg :: MonadSample m => m Int
agg = do
  x <- uniformD [0, 1]
  y <- uniformD [2, 1]
  return (x + y)

passed1, passed2, passed3, passed4, passed5, passed6, passed7, passed8, passed9, passed10 :: Bool


passed1 = enumerateWith (fromList [3, 1, 2]) agg ~== [(2,0.5),(1,0.25),(3,0.25)]

passed2 = enumerateWith (fromList [True, False]) (Sprinkler.hard)
  ~== enumerateWith (fromList [True, False]) (Sprinkler.soft)

passed3 =
  expectation (fmap ((**2) . (+ 1)) $ uniformD [0 , 1]) == 2.5

passed4 = volume (uniformD [1,2]) ~== 1.0

passed5 = True 
-- enumerateWith (fromList [0,1 :: Int]) (empirical [0 :: Int ,1 ,1, 1]) == [(1,0.75), (0,0.25)]

passed6 = (~== [(2,0.5),(3,0.5),(1,0.0)]) $
   enumerateWith (fromList [1,2,3]) $ do
    x <- uniformD [1 :: Int,2,3]
    condition (x > 1)
    return x

passed7 = (~== [(True,0.75),(False,0.25)]) $ 
  enumerateWith (fromList [True, False]) $ do
    x <- bernoulli 0.5
    factor $ if x then 0.3 else 0.1
    return x

passed8 = (==1) $ volume $ fmap (ln . exp . snd) $ runWeighted $ do
  x <- bernoulli 0.5
  factor $ if x then 0.2 else 0.1
  return x

passed9 = probability (1, 1000) (normal 1 10) - 0.5 < 0.05

passed10 = cdf (normal 5 5) 5 - 0.5 < 0.05