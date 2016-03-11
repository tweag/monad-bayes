import Test.Hspec

import qualified TestWeighted
import qualified TestDist
import qualified TestEmpirical

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
  describe "Empirical" $ do
    context "controlling population" $ do
      it "preserves the population when not expicitly altered" $ do
        TestEmpirical.pop_size `shouldBe` 5
      it "multiplies the number of samples when spawn invoked twice" $ do
        TestEmpirical.many_size `shouldBe` 15
    context "checking properties of samples" $ do
      it "correctly checks if all particles satisfy a property" $ do
        TestEmpirical.all_check `shouldBe` True
    context "distribution-preserving transformations" $ do
      it "transform preserves the distribution" $ do
        TestEmpirical.trans_check1 `shouldBe` True
        TestEmpirical.trans_check2 `shouldBe` True
      it "resample preserves the distribution" $ do
        TestEmpirical.resample_check 1 `shouldBe` True
        TestEmpirical.resample_check 2 `shouldBe` True
