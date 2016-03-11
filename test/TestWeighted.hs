module TestWeighted where

import Test.Hspec
import Data.AEq
import System.Random
import Control.Monad.State
import Data.Number.LogFloat
import Data.List

import Base
import Sampler
import Inference

model :: MonadBayes m => m (Int,Double)
model = do
  n <- uniformD [0,1,2]
  unless (n == 0) (factor 0.5)
  x <- if (n == 0) then return 1 else normal 0 1
  when (n == 2) (factor $ logFloat (x*x))
  return (n,x)

result :: MonadDist m => m ((Int,Double),LogFloat)
result = importance model

gs :: [StdGen]
gs = take 100 $ unfoldr (Just . split) $ mkStdGen 0

samples :: [((Int,Double),LogFloat)]
samples = map (stdSample result) gs

passed = all id $ map check samples

check :: ((Int,Double),LogFloat) -> Bool
check ((0,1),1) = True
check ((1,x),y) = fromLogFloat y ~== 0.5
check ((2,x),y) = fromLogFloat y ~== 0.5 * x * x
check _ = False
