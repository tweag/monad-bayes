{-# LANGUAGE
  TypeFamilies
 #-}

module TestWeighted where

import Test.Hspec
import Data.AEq
import System.Random
import Control.Monad.State
import Data.List

import Control.Monad.Bayes.LogDomain (LogDomain, fromLogDomain, toLogDomain)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Inference

model :: (MonadBayes m, CustomReal m ~ Double) => m (Int,Double)
model = do
  n <- uniformD [0,1,2]
  unless (n == 0) (factor 0.5)
  x <- if (n == 0) then return 1 else normal 0 1
  when (n == 2) (factor $ toLogDomain (x*x))
  return (n,x)

result :: (MonadDist m, CustomReal m ~ Double) => m ((Int,Double),LogDomain Double)
result = importance model

gs :: [StdGen]
gs = take 100 $ unfoldr (Just . split) $ mkStdGen 0

samples :: [((Int,Double),LogDomain Double)]
samples = map (stdSample result) gs

passed = all id $ map check samples

check :: ((Int,Double),LogDomain Double) -> Bool
check ((0,1),1) = True
check ((1,x),y) = fromLogDomain y ~== 0.5
check ((2,x),y) = fromLogDomain y ~== 0.5 * x * x
check _ = False
