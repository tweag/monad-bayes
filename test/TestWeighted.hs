{-# LANGUAGE
  TypeFamilies
 #-}

module TestWeighted where

import Data.AEq
import Control.Monad.State
import Data.Bifunctor (second)
import Numeric.Log

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted

model :: MonadInfer m => m (Int,Double)
model = do
  n <- uniformD [0,1,2]
  unless (n == 0) (factor 0.5)
  x <- if n == 0 then return 1 else normal 0 1
  when (n == 2) (factor $ (Exp . log) (x*x))
  return (n,x)

result :: MonadSample m => m ((Int,Double), Double)
result = second (exp . ln) <$> runWeighted model

passed :: IO Bool
passed = fmap check (sampleIOfixed result)

check :: ((Int,Double), Double) -> Bool
check ((0,1),1) = True
check ((1,_),y) =  y ~== 0.5
check ((2,x),y) =  y ~== 0.5 * x * x
check _ = False
