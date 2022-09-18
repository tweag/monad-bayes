{-# LANGUAGE ImportQualifiedPost #-}

import Data.AEq (AEq ((~==)))
import Test.Hspec (context, describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (ioProperty, property, (==>))
import TestAdvanced qualified
import TestDistribution qualified
import TestEnumerator qualified
import TestInference qualified
import TestIntegrator qualified
import TestPipes (hmms)
import TestPipes qualified
import TestPopulationT qualified
import TestSampler qualified
import TestSequentialT qualified
import TestStormerVerlet qualified
import TestWeighted qualified

main :: IO ()
main = hspec do
  describe "Stormer Verlet" $
    it "conserves energy" $
      do
        p1 <- TestStormerVerlet.passed1
        p1 `shouldBe` True
  describe "Distribution" $
    it "gives correct mean, variance and covariance" $
      do
        p1 <- TestDistribution.passed1
        p1 `shouldBe` True
        p2 <- TestDistribution.passed2
        p2 `shouldBe` True
        p3 <- TestDistribution.passed3
        p3 `shouldBe` True
  describe "Weighted" $
    it "accumulates likelihood correctly" $
      do
        passed <- TestWeighted.passed
        passed `shouldBe` True
  describe "Enumerator" do
    it "sorts samples and aggregates weights" $
      TestEnumerator.passed2 `shouldBe` True
    it "gives correct answer for the sprinkler model" $
      TestEnumerator.passed3 `shouldBe` True
    it "computes expectation correctly" $
      TestEnumerator.passed4 `shouldBe` True
  describe "Integrator Expectation" do
    prop "expectation numerically" $
      \mean var ->
        var > 0 ==> property $ TestIntegrator.normalExpectation mean (sqrt var) ~== mean
  describe "Integrator Variance" do
    prop "variance numerically" $
      \mean var ->
        var > 0 ==> property $ TestIntegrator.normalVariance mean (sqrt var) ~== var
  describe "Sampler mean and variance" do
    it "gets right mean and variance" $
      TestSampler.testMeanAndVariance `shouldBe` True
  describe "Integrator Volume" do
    prop "volume sums to 1" $
      property $ \case
        [] -> True
        ls -> (TestIntegrator.volumeIsOne ls)

  describe "Integrator" do
    it "" $
      all
        (== True)
        [ TestIntegrator.passed1,
          TestIntegrator.passed2,
          TestIntegrator.passed3,
          TestIntegrator.passed4,
          TestIntegrator.passed5,
          TestIntegrator.passed6,
          TestIntegrator.passed7,
          TestIntegrator.passed8,
          TestIntegrator.passed9,
          TestIntegrator.passed10,
          TestIntegrator.passed11,
          TestIntegrator.passed12,
          TestIntegrator.passed13,
          TestIntegrator.passed14
        ]
        `shouldBe` True

  describe "PopulationT" do
    context "controlling population" do
      it "preserves the population when not explicitly altered" do
        popSize <- TestPopulationT.popSize
        popSize `shouldBe` 5
      it "multiplies the number of samples when spawn invoked twice" do
        manySize <- TestPopulationT.manySize
        manySize `shouldBe` 15
      it "correctly computes population average" $
        TestPopulationT.popAvgCheck `shouldBe` True
    context "distribution-preserving transformations" do
      it "collapse preserves the distribution" do
        TestPopulationT.transCheck1 `shouldBe` True
        TestPopulationT.transCheck2 `shouldBe` True
      it "resample preserves the distribution" do
        TestPopulationT.resampleCheck 1 `shouldBe` True
        TestPopulationT.resampleCheck 2 `shouldBe` True
  describe "SequentialT" do
    it "stops at every factor" do
      TestSequentialT.checkTwoSync 0 `shouldBe` True
      TestSequentialT.checkTwoSync 1 `shouldBe` True
      TestSequentialT.checkTwoSync 2 `shouldBe` True
    it "preserves the distribution" $
      TestSequentialT.checkPreserve `shouldBe` True
    it "produces correct intermediate weights" do
      TestSequentialT.checkSync 0 `shouldBe` True
      TestSequentialT.checkSync 1 `shouldBe` True
      TestSequentialT.checkSync 2 `shouldBe` True
  describe "SMC" do
    it "terminates" $
      seq TestInference.checkTerminateSMC () `shouldBe` ()
    it "preserves the distribution on the sprinkler model" $
      TestInference.checkPreserveSMC `shouldBe` True
    prop "number of particles is equal to its second parameter" $
      \observations particles ->
        observations >= 0 && particles >= 1 ==> ioProperty do
          checkParticles <- TestInference.checkParticles observations particles
          return $ checkParticles == particles
  describe "SMC with systematic resampling" $
    prop "number of particles is equal to its second parameter" $
      \observations particles ->
        observations >= 0 && particles >= 1 ==> ioProperty do
          checkParticles <- TestInference.checkParticlesSystematic observations particles
          return $ checkParticles == particles
  describe "Equivalent Expectations" do
    prop "Gamma Normal" $
      ioProperty . TestInference.testGammaNormal
    prop "Normal Normal" $
      \n -> ioProperty (TestInference.testNormalNormal [max (-3) $ min 3 n])
    prop "Beta Bernoulli" $
      ioProperty . TestInference.testBetaBernoulli
  describe "Pipes: Urn" do
    it "Distributions are equivalent" do
      TestPipes.urns 10 `shouldBe` True
  describe "Pipes: HMM" do
    prop "pipe model is equivalent to standard model" $
      \num -> property $ hmms $ take 5 num

  describe "SMC with stratified resampling" $
    prop "number of particles is equal to its second parameter" $
      \observations particles ->
        observations >= 0 && particles >= 1 ==> ioProperty do
          checkParticles <- TestInference.checkParticlesStratified observations particles
          return $ checkParticles == particles

  describe "Expectation from all inference methods" $
    it "gives correct answer for the sprinkler model" do
      passed1 <- TestAdvanced.passed1
      passed1 `shouldBe` True
      passed2 <- TestAdvanced.passed2
      passed2 `shouldBe` True
      passed3 <- TestAdvanced.passed3
      passed3 `shouldBe` True
      passed4 <- TestAdvanced.passed4
      passed4 `shouldBe` True
      passed5 <- TestAdvanced.passed5
      passed5 `shouldBe` True
      passed6 <- TestAdvanced.passed6
      passed6 `shouldBe` True
      passed7 <- TestAdvanced.passed7
      passed7 `shouldBe` True
