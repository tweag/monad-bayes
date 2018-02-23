import System.Random.MWC (createSystemRandom, GenIO)
import Data.Time
import Options.Applicative
import Data.Semigroup ((<>))

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Traced

import qualified HMM
import qualified LogReg
import qualified LDA

data Model = LR | HMM | LDA
  deriving(Read,Show)

getModel :: MonadInfer m => Model -> (Int, m String)
getModel LR = (length LogReg.xs, show <$> LogReg.logisticRegression (zip LogReg.xs LogReg.labels))
getModel HMM = (length HMM.values, show <$> HMM.hmm HMM.values)
getModel LDA = (length (concat LDA.docs), show <$> LDA.lda LDA.docs)

data Alg = SMC | MH | RMSMC
  deriving(Read,Show)

runAlg :: Model -> Alg -> SamplerIO String
runAlg model alg =
  case alg of
    SMC ->
      let n = 100
          (k, m) = getModel model
      in show <$> (runPopulation $ smcSystematic k n m)
    MH  ->
      let t = 100
          (_, m) = getModel model
      in show <$> (prior $ mh t m)
    RMSMC ->
      let n = 10
          t = 1
          (k, m) = getModel model
      in show <$> (runPopulation $ rmsmcLocal k n t m)


infer :: Model -> Alg -> IO ()
infer model alg = do
  g <- createSystemRandom
  x <- sampleIOwith (runAlg model alg) g
  print x

opts :: ParserInfo (Model, Alg)
opts = flip info fullDesc $ liftA2 (,) model alg where
  model = option auto
          ( long "model"
          <> short 'm'
          <> help "Model")
  alg = option auto
          ( long "alg"
         <> short 'a'
         <> help "Inference algorithm")

main :: IO ()
main = do
  (model, alg) <- execParser opts

  startTime <- getCurrentTime

  infer model alg

  endTime <- getCurrentTime
  print (diffUTCTime endTime startTime)
