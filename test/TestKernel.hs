{-# LANGUAGE
  FlexibleInstances
 #-}

module TestKernel where

import Numeric.LinearAlgebra
import Data.AEq
import Data.Bifunctor (second)

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad.Bayes.Kernel


spec :: Spec
spec = do
  describe "Kernel methods" $ do
    it "kernels are symmetric" $ property $ prop_symmetricKernel
    it "kernels are positive semidefinite" $
        property prop_positiveSemidefiniteKernel
    it "kernel matrix has correct size" $ property prop_kernelMatrixSize
    it "kernel matrix contains the right values" $
        property prop_kernelMatrixElement
    it "RKHS product is symmetric" $ property prop_rkhsProductSymmetric
    it "RKHS product is linear" $ property prop_rkhsProductLinear
    it "MMD is symmetric" $ property prop_mmdSymmetric
    it "MMD is non-negative" $ property prop_mmdNonnegative
    it "MMD is zero between a distribution and itself" $
        property prop_mmdZeroOnEqual

instance Arbitrary (Kernel R R) where
  arbitrary = oneof [constKer, gaussKer] where
    constKer = fmap constant $ choose (0, 100)
    gaussKer = fmap gaussian $ choose (0, 100)

instance Show (Kernel R R) where
  show _ = "<<kernel>>"

prop_symmetricKernel :: Kernel R R -> R -> R -> Bool
prop_symmetricKernel k x y = evalKernel k x y ~== evalKernel k y x

prop_positiveSemidefiniteKernel :: Kernel R R -> [(R,R)] -> Bool
prop_positiveSemidefiniteKernel k p =
  sum [w * w' * evalKernel k x x' | (x,w) <- p, (x',w') <- p] >= 0

prop_kernelMatrixSize :: Kernel R R -> [R] -> [R] -> Bool
prop_kernelMatrixSize k p q = size (kernelMatrix k p q) == (length p, length q)

prop_kernelMatrixElement :: Kernel R R -> [R] -> [R] -> Bool
prop_kernelMatrixElement k p q = let kmat = kernelMatrix k p q in
  all id [kmat ! i ! j == evalKernel k (p !! i) (q !! j) |
          i <- [0..length p - 1], j <- [0.. length q - 1]]

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
