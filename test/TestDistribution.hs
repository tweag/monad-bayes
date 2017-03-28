module TestDistribution where

import Test.Hspec
import Test.QuickCheck

import Numeric.LinearAlgebra
import Data.AEq

import Numeric.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Distribution

spec :: Spec
spec = do
  describe "Multivariate normal distribution" $ do
    it "computes correct density with symmetric covariance" $ property $ prop_density

newtype MVNParam = MVNParam (Vector R, Vector R, Vector R)
  deriving (Show)

instance Arbitrary MVNParam where
  arbitrary = do
    k <- choose (1,1)
    ms <- sequence $ replicate k $ choose (-10,10)
    vs <- sequence $ replicate k $ choose (0.1,10)
    xs <- sequence $ replicate k $ choose (-100,100)
    return $ MVNParam (fromList ms, fromList vs, fromList xs)

prop_density :: MVNParam -> Bool
prop_density (MVNParam (m, v, x)) = toLog mvn_density ~== toLog normal_density where
  mvn_density = pdf (mvnormalDist m (trustSym (diag v))) x
  normal_density =
    product $ map (\i -> pdf (normalDist (m ! i) (sqrt (v ! i))) (x ! i)) [0 .. size m - 1]
