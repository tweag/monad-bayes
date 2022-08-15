{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Monad.Bayes.Class (MonadInfer)
import Control.Monad.Bayes.Inference.MCMC (MCMCConfig (..), Proposal (SingleSiteMH))
import Control.Monad.Bayes.Inference.RMSMC (rmsmcBasic)
import Control.Monad.Bayes.Inference.SMC
  ( SMCConfig (SMCConfig, numParticles, numSteps, resampler),
    smc,
  )
import Control.Monad.Bayes.Population
  ( population,
    resampleSystematic,
  )
import Control.Monad.Bayes.Sampler
  ( Sampler,
    sampleSTfixed,
    sampleWith,
  )
import Control.Monad.Bayes.Traced (mh)
import Control.Monad.Bayes.Weighted (unweighted)
import Control.Monad.ST (runST)
import Data.Time (diffUTCTime, getCurrentTime)
import HMM qualified
import LDA qualified
import LogReg qualified
import Options.Applicative
  ( Applicative (liftA2),
    ParserInfo,
    auto,
    execParser,
    fullDesc,
    help,
    info,
    long,
    maybeReader,
    option,
    short,
  )
import System.Random.MWC (GenIO, createSystemRandom)

data Model = LR Int | HMM Int | LDA (Int, Int)
  deriving stock (Show, Read)

parseModel :: String -> Maybe Model
parseModel s =
  case s of
    'L' : 'R' : n -> Just $ LR (read n)
    'H' : 'M' : 'M' : n -> Just $ HMM (read n)
    'L' : 'D' : 'A' : n -> Just $ LDA (5, read n)
    _ -> Nothing

getModel :: MonadInfer m => Model -> (Int, m String)
getModel model = (size model, program model)
  where
    size (LR n) = n
    size (HMM n) = n
    size (LDA (d, w)) = d * w
    program (LR n) = show <$> (LogReg.logisticRegression (runST $ sampleSTfixed (LogReg.syntheticData n)))
    program (HMM n) = show <$> (HMM.hmm (runST $ sampleSTfixed (HMM.syntheticData n)))
    program (LDA (d, w)) = show <$> (LDA.lda (runST $ sampleSTfixed (LDA.syntheticData d w)))

data Alg = SMC | MH | RMSMC
  deriving stock (Read, Show)

runAlg :: Model -> Alg -> Sampler GenIO IO String
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

infer :: Model -> Alg -> IO ()
infer model alg = do
  g <- createSystemRandom
  x <- sampleWith (runAlg model alg) g
  print x

opts :: ParserInfo (Model, Alg)
opts = flip info fullDesc $ liftA2 (,) model alg
  where
    model =
      option
        (maybeReader parseModel)
        ( long "model"
            <> short 'm'
            <> help "Model"
        )
    alg =
      option
        auto
        ( long "alg"
            <> short 'a'
            <> help "Inference algorithm"
        )

main :: IO ()
main = do
  (model, alg) <- execParser opts
  startTime <- getCurrentTime
  infer model alg
  endTime <- getCurrentTime
  print (diffUTCTime endTime startTime)
