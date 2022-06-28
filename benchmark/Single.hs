{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Population (population)
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted
import Data.Time
import HMM qualified
import LDA qualified
import LogReg qualified
import Options.Applicative
import System.Random.MWC (createSystemRandom)

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
    synthesize :: SamplerST a -> (a -> b) -> b
    synthesize dataGen prog = prog (sampleSTfixed dataGen)
    program (LR n) = show <$> synthesize (LogReg.syntheticData n) LogReg.logisticRegression
    program (HMM n) = show <$> synthesize (HMM.syntheticData n) HMM.hmm
    program (LDA (d, w)) = show <$> synthesize (LDA.syntheticData d w) LDA.lda

data Alg = SMC | MH | RMSMC
  deriving stock (Read, Show)

runAlg :: Model -> Alg -> SamplerIO String
runAlg model alg =
  case alg of
    SMC ->
      let n = 100
          (k, m) = getModel model
       in show <$> population (smcSystematic k n m)
    MH ->
      let t = 100
          (_, m) = getModel model
       in show <$> unweighted (mh t m)
    RMSMC ->
      let n = 10
          t = 1
          (k, m) = getModel model
       in show <$> population (rmsmcBasic k n t m)

infer :: Model -> Alg -> IO ()
infer model alg = do
  g <- createSystemRandom
  x <- sampleIOwith (runAlg model alg) g
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
