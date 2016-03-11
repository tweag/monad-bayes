import Test.Hspec

import qualified TestWeighted

main :: IO ()
main = hspec $ do
  describe "Weighted" $ do
    it "accumulates likelihood correctly" $ do
      TestWeighted.passed `shouldBe` True
