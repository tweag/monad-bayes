import Test.Hspec

import qualified TestWeighted
import qualified TestDist

main :: IO ()
main = hspec $ do
  describe "Weighted" $ do
    it "accumulates likelihood correctly" $ do
      TestWeighted.passed `shouldBe` True
  describe "Dist" $ do
    it "normalizes categorical" $ do
      TestDist.passed1 `shouldBe` True
    it "sorts samples and aggregates weights" $ do
      TestDist.passed2 `shouldBe` True
    it "gives correct answer for the sprinkler model" $ do
      TestDist.passed3 `shouldBe` True
