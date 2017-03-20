import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified TestWeighted
import qualified TestEnumerator
import qualified TestPopulation
import qualified TestSequential
import qualified TestTrace
import qualified TestInference
import qualified TestSMCObservations
import qualified TestGradient
import qualified TestConditional
import qualified TestDistribution

main :: IO ()
main = hspec $ do
  describe "Weighted" $ do
    it "accumulates likelihood correctly" $ do
      passed <- TestWeighted.passed
      passed `shouldBe` True
  describe "Dist" $ do
    it "normalizes categorical" $ do
      TestEnumerator.passed1 `shouldBe` True
    it "sorts samples and aggregates weights" $ do
      TestEnumerator.passed2 `shouldBe` True
    it "gives correct answer for the sprinkler model" $ do
      TestEnumerator.passed3 `shouldBe` True
    it "computes expectation correctly" $ do
      TestEnumerator.passed4 `shouldBe` True
  describe "Empirical" $ do
    context "controlling population" $ do
      it "preserves the population when not expicitly altered" $ do
        pop_size <- TestPopulation.pop_size
        pop_size `shouldBe` 5
      it "multiplies the number of samples when spawn invoked twice" $ do
        many_size <- TestPopulation.many_size
        many_size `shouldBe` 15
      it "correctly computes population average" $ do
        TestPopulation.popAvg_check `shouldBe` True
--    context "checking properties of samples" $ do
--      it "correctly checks if all particles satisfy a property" $ do
--        TestPopulation.all_check `shouldBe` True
    context "distribution-preserving transformations" $ do
      it "transform preserves the distribution" $ do
        TestPopulation.trans_check1 `shouldBe` True
        TestPopulation.trans_check2 `shouldBe` True
      it "resample preserves the distribution" $ do
        TestPopulation.resample_check 1 `shouldBe` True
        TestPopulation.resample_check 2 `shouldBe` True
  describe "Particle" $ do
    it "stops at every factor" $ do
      TestSequential.check_two_sync 0 `shouldBe` True
      TestSequential.check_two_sync 1 `shouldBe` True
      TestSequential.check_two_sync 2 `shouldBe` True
    it "preserves the distribution" $ do
      TestSequential.check_preserve `shouldBe` True
    it "produces correct intermediate weights" $ do
      TestSequential.check_sync 0 `shouldBe` True
      TestSequential.check_sync 1 `shouldBe` True
      TestSequential.check_sync 2 `shouldBe` True
  -- describe "Trace" $ do
  --   context "RandomDB = [Cache]" $ do
  --     it "correctly records values" $ do
  --       TestTrace.check_writing `shouldBe` True
  --     it "correctly reuses values" $ do
  --       TestTrace.check_reading `shouldBe` True
  --   it "has reuse ratio 1 on an empty database" $ do
  --     TestTrace.check_reuse_ratio TestTrace.m            `shouldBe` True
  --     TestTrace.check_reuse_ratio TestSequential.sprinkler `shouldBe` True
  describe "Density" $ do
    it "correctly evaluates conditional distribution" $ do
      TestConditional.check_missing_conditional `shouldBe` True
      TestConditional.check_longer_conditional `shouldBe` True
    it "correctly computes pseudo-marginal density" $ do
      TestConditional.check_first_density `shouldBe` True
    it "correctly computes joint density" $ do
      TestConditional.check_joint_density_true `shouldBe` True
      TestConditional.check_joint_density_false `shouldBe` True
  describe "SMC" $ do
    it "terminates" $ do
      seq TestInference.check_terminate_smc () `shouldBe` ()
    it "preserves the distribution on the sprinkler model" $ do
      TestInference.check_preserve_smc `shouldBe` True
    prop "number of particles is equal to its second parameter" $
      \observations particles ->
        observations >= 0 && particles >= 1 ==> ioProperty $ do
          check_particles <- TestInference.check_particles observations particles
          return $ check_particles == particles
  describe "MH" $ do
    -- it "MH from prior leaves posterior invariant" $ do
    --   TestInference.check_prior_trans `shouldBe` True
    -- it "Trace MH produces correct number of samples" $ do
    --   trace_mh_length <- TestInference.trace_mh_length 11
    --   trace_mh_length `shouldBe` 11
    -- it "Trace MH leaves posterior invariant" $ do
    --   TestInference.check_trace_trans `shouldBe` True
    -- it "Trace MH leaves posterior invariant when the model has shifting support" $ do
    --   TestInference.check_trace_support `shouldBe` True
    it "Custom MH preserves posterior distribution" $ do
      TestInference.check_custom_mh `shouldBe` True
  -- describe "Population/Trace/Particle hybrids" $ do
  --   it "ISMH preserves the posterior on the sprinkler model" $ do
  --     TestInference.check_preserve_ismh `shouldBe` True
  --   it "SMH preserves the posterior on the sprinkler model" $ do
  --     TestInference.check_preserve_smh `shouldBe` True
  --   it "Resample-move SMC preserves the posterior on the sprinkler model" $ do
  --     TestInference.check_preserve_smcrm `shouldBe` True

    -- too large to execute
    -- it "PIMH leaves posterior invariant" $ do
    --   TestInference.check_pimh_trans `shouldBe` True
  -- describe "Number of SMC observations sufficient for each models" $ do
  --   check_smc_observations 5 "Gamma.model" Gamma.model
  --   check_smc_observations 0 "Gamma.exact" Gamma.exact
  --   check_smc_observations 0 "Dice.dice" (Dice.dice 4)
  --   check_smc_observations 1 "Dice.dice_soft" Dice.dice_soft
  --   check_smc_observations 1 "Dice.dice_hard" Dice.dice_hard
  --   check_smc_observations 0 "BetaBin.latent" (BetaBin.latent 5)
  --   check_smc_observations 0 "BetaBin.urn" (BetaBin.urn 5)
  --   check_smc_observations 16 "HMM.hmm" HMM.hmm
  describe "Density computation" $ do
    it "gives correct value on gamma-normal-beta model" $ do
      TestGradient.check_density `shouldBe` True
    it "gives correct gradient on gamma-normal-beta model" $ do
      TestGradient.check_gradient `shouldBe` True
  TestDistribution.spec

check_smc_observations n modelName model =
    it (show n ++ " observations for " ++ modelName) $ do
      check_smc_weight <- TestSMCObservations.check_smc_weight n 30 model
      check_smc_weight `shouldBe` True
