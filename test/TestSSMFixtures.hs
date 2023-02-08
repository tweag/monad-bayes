module TestSSMFixtures where

import Control.Monad.Bayes.Class (MonadDistribution)
import Control.Monad.Bayes.Inference.MCMC
import Control.Monad.Bayes.Inference.PMMH as PMMH (pmmh)
import Control.Monad.Bayes.Inference.RMSMC (rmsmc, rmsmcBasic, rmsmcDynamic)
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.SMC2 as SMC2 (smc2)
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler.Strict (sampleIOfixed)
import Control.Monad.Bayes.Weighted (unweighted)
import NonlinearSSM
import System.IO (readFile')
import System.IO.Error (catchIOError, isDoesNotExistError)
import Test.Hspec

data Alg = SMC | RMSMC | RMSMCDynamic | RMSMCBasic | PMMH | SMC2
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

algs :: [Alg]
algs = [minBound .. maxBound]

fixtureToFilename :: Alg -> FilePath
fixtureToFilename alg = "test/fixtures/SSM-" ++ show alg ++ ".txt"

type SSMData = [Double]

t :: Int
t = 5

-- FIXME refactor such that it can be reused in ssm benchmark
runAlgFixed :: MonadDistribution m => SSMData -> Alg -> m String
runAlgFixed ys SMC = fmap show $ population $ smc SMCConfig {numSteps = t, numParticles = 10, resampler = resampleMultinomial} (param >>= model ys)
runAlgFixed ys RMSMC =
  fmap show $
    population $
      rmsmc
        MCMCConfig {numMCMCSteps = 10, numBurnIn = 0, proposal = SingleSiteMH}
        SMCConfig {numSteps = t, numParticles = 10, resampler = resampleSystematic}
        (param >>= model ys)
runAlgFixed ys RMSMCBasic =
  fmap show $
    population $
      rmsmcBasic
        MCMCConfig {numMCMCSteps = 10, numBurnIn = 0, proposal = SingleSiteMH}
        SMCConfig {numSteps = t, numParticles = 10, resampler = resampleSystematic}
        (param >>= model ys)
runAlgFixed ys RMSMCDynamic =
  fmap show $
    population $
      rmsmcDynamic
        MCMCConfig {numMCMCSteps = 10, numBurnIn = 0, proposal = SingleSiteMH}
        SMCConfig {numSteps = t, numParticles = 10, resampler = resampleSystematic}
        (param >>= model ys)
runAlgFixed ys PMMH =
  fmap show $
    unweighted $
      pmmh
        MCMCConfig {numMCMCSteps = 2, numBurnIn = 0, proposal = SingleSiteMH}
        SMCConfig {numSteps = t, numParticles = 3, resampler = resampleSystematic}
        param
        (model ys)
runAlgFixed ys SMC2 = fmap show $ population $ smc2 t 3 2 1 param (model ys)

testFixture :: Alg -> SpecWith ()
testFixture alg = do
  let filename = fixtureToFilename alg
  it ("should agree with the fixture " ++ filename) $ do
    ys <- sampleIOfixed $ generateData t
    fixture <- catchIOError (readFile' filename) $ \e ->
      if isDoesNotExistError e
        then return ""
        else ioError e
    sampled <- sampleIOfixed $ runAlgFixed (map fst ys) alg
    -- Reset in case of fixture update or creation
    writeFile filename sampled
    fixture `shouldBe` sampled

test :: SpecWith ()
test = describe "TestSSMFixtures" $ mapM_ testFixture algs
