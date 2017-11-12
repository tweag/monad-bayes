import Criterion.Main
import Criterion.Types
import System.Random.MWC (createSystemRandom, GenIO)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Traced

-- import NonlinearSSM
import qualified HMM
import qualified LogReg
import qualified LDA


data ProbProgSys = MonadBayes | Anglican | WebPPL
  deriving(Show)

data Model = LR [(Double,Bool)] | HMM [Double] | LDA [[String]]
instance Show Model where
  show (LR xs) = "LR" ++ show (length xs)
  show (HMM xs) = "HMM" ++ show (length xs)
  show (LDA xs) = "LDA" ++ show (length $ head xs)
buildModel :: MonadInfer m => Model -> m String
buildModel (LR dataset) = show <$> LogReg.logisticRegression dataset
buildModel (HMM dataset) = show <$> HMM.hmm dataset
buildModel (LDA dataset) = show <$> LDA.lda dataset
modelLength :: Model -> Int
modelLength (LR xs) = length xs
modelLength (HMM xs) = length xs
modelLength (LDA xs) = length xs

data Alg = MH Int | SMC Int | RMSMC Int Int
instance Show Alg where
  show (MH n) = "MH" ++ show n
  show (SMC n) = "SMC" ++ show n
  show (RMSMC n t) = "SMCRM" ++ show n ++ "-" ++ show t
runAlg :: Model -> Alg -> SamplerIO String
runAlg model (MH n) = show <$> prior (mh n (buildModel model))
runAlg model (SMC n) = show <$> runPopulation (smcSystematic (modelLength model) n (buildModel model))
runAlg model (RMSMC n t) = show <$> runPopulation (rmsmc (modelLength model) n t (buildModel model))

prepareBenchmarkable :: GenIO -> ProbProgSys -> Model -> Alg -> Benchmarkable
prepareBenchmarkable g MonadBayes model alg = nfIO $ sampleIOwith (runAlg model alg) g
prepareBenchmarkable _ Anglican _ _  = error "Anglican benchmarks not available"
prepareBenchmarkable _ WebPPL _ _ = error "WebPPL benchmarks not available"

prepareBenchmark :: GenIO -> ProbProgSys -> Model -> Alg -> Benchmark
prepareBenchmark g system model alg =
  bench (show system ++ sep ++ show model ++ sep ++ show alg) $
  prepareBenchmarkable g system model alg where
    sep = "_"

main :: IO ()
main = do

  g <- createSystemRandom

  let systems = [MonadBayes]

  let lrData = replicate 100 (0.0, True) --TODO: draw some non-trivial datapoints
  let lrLengths = [1, 3, 10]
  let hmmData = replicate 100 0 -- TODO: draw some non-trivial datapoints
  let hmmLengths = [1, 3, 10]
  let ldaData = replicate 5 (replicate 100 "wolf") -- TODO: draw some non-trivial datapoints
  let ldaLengths = [1, 3]--, 10]

  let models = map (LR . (`take` lrData)) lrLengths ++
               map (HMM . (`take` hmmData)) hmmLengths ++
               map (\n -> LDA $ map (take n) ldaData) ldaLengths

  let algs = [MH 10, SMC 10, RMSMC 10 1]

  let benchmarks = map (uncurry3 (prepareBenchmark g)) xs where
        uncurry3 f (x,y,z) = f x y z
        xs = do
          s <- systems
          m <- models
          a <- algs
          return (s,m,a)

  let config = defaultConfig{csvFile = Just "speed.csv"}
  defaultMainWith config benchmarks
