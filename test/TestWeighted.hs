{-# LANGUAGE
  TypeFamilies
 #-}

module TestWeighted where

import Test.Hspec
import Data.AEq
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

result :: (MonadDist m, CustomReal m ~ Double) => m ((Int,Double), Double)
result = fmap head $ importance 1 model

passed = fmap check (sampleIOfixed result)

check :: ((Int,Double), Double) -> Bool
check ((0,1),1) = True
check ((1,x),y) =  y ~== 0.5
check ((2,x),y) =  y ~== 0.5 * x * x
check _ = False
