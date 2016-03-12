import Test.Hspec

import qualified TestWeighted
import qualified TestDist
import qualified TestEmpirical
import qualified TestParticle
import qualified TestTrace
import qualified TestInference

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
  describe "Particle" $ do
    it "stops at every factor" $ do
      TestParticle.check_two_sync 0 `shouldBe` True
      TestParticle.check_two_sync 1 `shouldBe` True
      TestParticle.check_two_sync 2 `shouldBe` True
    it "preserves the distribution" $ do
      TestParticle.check_preserve `shouldBe` True
    it "produces correct intermediate weights" $ do
      TestParticle.check_sync 0 `shouldBe` True
      TestParticle.check_sync 1 `shouldBe` True
      TestParticle.check_sync 2 `shouldBe` True
  describe "Trace" $ do
    context "RandomDB = [Cache]" $ do
      it "correctly records values" $ do
        TestTrace.check_writing `shouldBe` True
      it "correctly reuses values" $ do
        TestTrace.check_reading `shouldBe` True
  describe "SMC" $ do
    it "terminates" $ do
      seq TestInference.check_terminate_smc () `shouldBe` ()
    it "preserves the distribution on the sprinkler model" $ do
      TestInference.check_preserve_smc `shouldBe` True
  describe "MH" $ do
    it "MH from prior leaves posterior invariant" $ do
      TestInference.check_prior_trans `shouldBe` True
    it "Trace MH leaves posterior invariant" $ do
      TestInference.check_trace_trans `shouldBe` True
    -- too large to execute
    -- it "PIMH leaves posterior invariant" $ do
    --   TestInference.check_pimh_trans `shouldBe` True
