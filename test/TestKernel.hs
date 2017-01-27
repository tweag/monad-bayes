{-# LANGUAGE
  FlexibleInstances
 #-}

module TestKernel where

import Numeric.LinearAlgebra
import Data.AEq
import Test.QuickCheck
import Data.Bifunctor (second)

import Control.Monad.Bayes.Kernel

instance Arbitrary (Kernel R R) where
  arbitrary = oneof [constKer, gaussKer] where
    constKer = fmap constant $ choose (0, 100)
    gaussKer = fmap gaussian $ choose (0, 100)

instance Show (Kernel R R) where
  show _ = "kernel"

prop_symmetricKernel :: Kernel R R -> R -> R -> Bool
prop_symmetricKernel k x y = evalKernel k x y ~== evalKernel k y x

symmetricKernel :: Property
symmetricKernel = verbose $ property prop_symmetricKernel

prop_positiveSemidefiniteKernel :: Kernel R R -> [(R,R)] -> Bool
prop_positiveSemidefiniteKernel k p =
  sum [w * w' * evalKernel k x x' | (x,w) <- p, (x',w') <- p] >= 0

prop_kernelMatrixSize :: Kernel R R -> [R] -> [R] -> Bool
prop_kernelMatrixSize k p q = size (kernelMatrix k p q) == (length p, length q)

prop_kernelMatrixElement :: Kernel R R -> [R] -> [R] -> Int -> Int -> Bool
prop_kernelMatrixElement k p q i j =
  kernelMatrix k p q ! i ! j == evalKernel k (p !! i) (q !! j)

prop_rkhsProductSymmetric :: Kernel R R -> [(R,R)] -> [(R,R)] -> Bool
prop_rkhsProductSymmetric k p q = rkhsProduct k p q ~== rkhsProduct k q p

prop_rkhsProductLinear :: Kernel R R -> [(R,R)] -> [(R,R)] -> R -> [(R,R)] -> Bool
prop_rkhsProductLinear k p q a x =
  rkhsProduct k p (map (second (* a)) q ++ x) ~==
    rkhsProduct k p q * a + rkhsProduct k p x

prop_mmdSymmetric :: Kernel R R -> [(R,R)] -> [(R,R)] -> Bool
prop_mmdSymmetric k p q = mmd k p q ~== mmd k q p

prop_mmdNonnegative :: Kernel R R -> [(R,R)] -> [(R,R)] -> Bool
prop_mmdNonnegative k p q = mmd k p q >= 0

prop_mmdZeroOnEqual :: Kernel R R -> [(R,R)] -> Bool
prop_mmdZeroOnEqual k p = mmd k p p ~== 0

-- unit tests for rkhsProduct and mmd
