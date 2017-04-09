module TestOptimization where

import Test.Hspec
import Data.AEq
import Data.Functor.Identity

import Numeric.AD.Mode.Reverse

import Numeric.Optimization.SGD
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Sampler

spec :: Spec
spec = do
  describe "Optimization algorithms" $ do
    describe "Stochastic Gradient Descent" $ do
      it "finds minimum of a quadratic function with the full gradient" $ min_quad
      it "finds minimum of a quadratic function with stochastic gradient" $ min_s_quad

f :: Num a => [a] -> a
f xs = sum $ zipWith (*) xs xs

gradf :: [Double] -> [(Double,Double)]
gradf xs = gradWith (,) f xs

noisyGradf :: [Double] -> SamplerST [(Double,Double)]
noisyGradf xs = sequence $ map (\(x,g) -> normal g 1 >>= (\y -> return (x,y))) (gradf xs)

xs0 :: [Double]
xs0 = [1, -20]

sgd_param :: SGDParam Double
sgd_param = SGDParam {learningRate = 0.1, decayRate = 1, steps = 1000}

min_quad :: Bool
min_quad = runIdentity (sgd sgd_param (return . gradf) xs0) ~== [0,0]

min_s_quad :: Bool
min_s_quad = sampleSTfixed (sgd sgd_param{decayRate=0.999} (return . gradf) xs0) ~== [0,0]
