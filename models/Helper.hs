{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Helper where

import Control.Monad.Bayes.Class (MonadMeasure)
import Control.Monad.Bayes.Inference.MCMC (MCMCConfig (..), Proposal (SingleSiteMH))
import Control.Monad.Bayes.Inference.RMSMC (rmsmcBasic)
import Control.Monad.Bayes.Inference.SMC
  ( SMCConfig (SMCConfig, numParticles, numSteps, resampler),
    smc,
  )
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.Bayes.Traced hiding (model)
import Control.Monad.Bayes.Weighted
import Control.Monad.ST (runST)
import HMM qualified
import LDA qualified
import LogReg qualified

data Model = LR Int | HMM Int | LDA (Int, Int)
  deriving stock (Show, Read)

parseModel :: String -> Maybe Model
parseModel s =
  case s of
    'L' : 'R' : n -> Just $ LR (read n)
    'H' : 'M' : 'M' : n -> Just $ HMM (read n)
    'L' : 'D' : 'A' : n -> Just $ LDA (5, read n)
    _ -> Nothing

serializeModel :: Model -> Maybe String
serializeModel (LR n) = Just $ "LR" ++ show n
serializeModel (HMM n) = Just $ "HMM" ++ show n
serializeModel (LDA (5, n)) = Just $ "LDA" ++ show n
serializeModel (LDA _) = Nothing

data Alg = SMC | MH | RMSMC
  deriving stock (Read, Show, Eq, Ord, Enum, Bounded)

getModel :: MonadMeasure m => Model -> (Int, m String)
getModel model = (size model, program model)
  where
    size (LR n) = n
    size (HMM n) = n
    size (LDA (d, w)) = d * w
    program (LR n) = show <$> (LogReg.logisticRegression (runST $ sampleSTfixed (LogReg.syntheticData n)))
    program (HMM n) = show <$> (HMM.hmm (runST $ sampleSTfixed (HMM.syntheticData n)))
    program (LDA (d, w)) = show <$> (LDA.lda (runST $ sampleSTfixed (LDA.syntheticData d w)))

runAlg :: Model -> Alg -> SamplerIO String
runAlg model alg =
  case alg of
    SMC ->
      let n = 100
          (k, m) = getModel model
       in show <$> population (smc SMCConfig {numSteps = k, numParticles = n, resampler = resampleSystematic} m)
    MH ->
      let t = 100
          (_, m) = getModel model
       in show <$> unweighted (mh t m)
    RMSMC ->
      let n = 10
          t = 1
          (k, m) = getModel model
       in show <$> population (rmsmcBasic MCMCConfig {numMCMCSteps = t, numBurnIn = 0, proposal = SingleSiteMH} (SMCConfig {numSteps = k, numParticles = n, resampler = resampleSystematic}) m)

runAlgFixed :: Model -> Alg -> IO String
runAlgFixed model alg = sampleIOfixed $ runAlg model alg
