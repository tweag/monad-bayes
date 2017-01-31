{-# LANGUAGE
  FlexibleInstances
 #-}

module TestHerding where

import Test.Hspec
import Test.QuickCheck
import Numeric.LinearAlgebra
import Data.List
import Data.AEq
import Data.Bifunctor

import Control.Monad.Bayes.Herding

import TestKernel ()

spec :: Spec
spec = do
  describe "Kernel herding" $ do
    it "smaller herd is a prefix of a larger one" $ property prop_herdingPrefix
    it "greedily minimizes MMD" $ property prop_herdingGreedy

prop_herdingPrefix :: Kernel R R -> [(R,R)] -> Int -> Int -> Property
prop_herdingPrefix k p i j = not (null p) ==> property $
  herding k p (abs i) `isPrefixOf` herding k p (abs i + abs j)

prop_herdingGreedy :: Kernel R R -> [(R,R)] -> Int -> Property
prop_herdingGreedy k p' n = not (null p') && sum (map snd p') /= 0 && n>0 ==> greedy ~== eval (x:xs) where
  p = normalize p'
  x:xs = reverse $ herding k p n
  greedy = minimum [eval (y:xs) | y <- fst (unzip p)]
  eval ys = mmd k p $ map (\y -> (y, 1 / fromIntegral (length ys))) ys

-- | Normalize the weights to sum to 1.
normalize :: Fractional p => [(a,p)] -> [(a,p)]
normalize xs = map (second (/ z)) xs where
  z = sum $ map snd xs
