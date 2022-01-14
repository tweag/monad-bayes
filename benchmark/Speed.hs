{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted
import Control.Monad.ST (RealWorld, stToIO)
import Criterion.Main
import Criterion.Types
import qualified HMM
import qualified LDA
import qualified LogReg
import System.Exit
import System.Process hiding (env)
import System.Random.Stateful

-- | Environment to execute benchmarks in.
newtype Env = Env { rng :: STGenM StdGen RealWorld }

data ProbProgSys = MonadBayes
  deriving (Show)

data Model = LR [(Double, Bool)] | HMM [Double] | LDA [[String]]

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
modelLength (LDA xs) = sum (map length xs)

data Alg = MH Int | SMC Int | RMSMC Int Int

instance Show Alg where
  show (MH n) = "MH" ++ show n
  show (SMC n) = "SMC" ++ show n
  show (RMSMC n t) = "RMSMC" ++ show n ++ "-" ++ show t

runAlg :: Model -> Alg -> SamplerIO String
runAlg model (MH n) = show <$> prior (mh n (buildModel model))
runAlg model (SMC n) = show <$> runPopulation (smcSystematic (modelLength model) n (buildModel model))
runAlg model (RMSMC n t) = show <$> runPopulation (rmsmcLocal (modelLength model) n t (buildModel model))

prepareBenchmarkable ::
  STGenM StdGen RealWorld ->
  ProbProgSys ->
  Model ->
  Alg ->
  Benchmarkable
prepareBenchmarkable g MonadBayes model alg = nfIO $ sampleIOwith (runAlg model alg) g

prepareBenchmark :: Env -> ProbProgSys -> Model -> Alg -> Benchmark
prepareBenchmark e MonadBayes model alg =
  bench (show MonadBayes ++ sep ++ show model ++ sep ++ show alg) $
    prepareBenchmarkable (rng e) MonadBayes model alg
  where
    sep = "_"

-- | Checks if the requested benchmark is implemented.
supported :: (ProbProgSys, Model, Alg) -> Bool
supported (_, _, RMSMC _ _) = False
supported _ = True

systems :: [ProbProgSys]
systems =
  [ MonadBayes
  ]

lengthBenchmarks :: Env -> [(Double, Bool)] -> [Double] -> [[String]] -> [Benchmark]
lengthBenchmarks e lrData hmmData ldaData = benchmarks
  where
    lrLengths = 10 : map (* 100) [1 .. 10]
    hmmLengths = 10 : map (* 100) [1 .. 10]
    ldaLengths = 5 : map (* 50) [1 .. 10]
    models =
      map (LR . (`take` lrData)) lrLengths
        ++ map (HMM . (`take` hmmData)) hmmLengths
        ++ map (\n -> LDA $ map (take n) ldaData) ldaLengths
    algs =
      [ MH 100,
        SMC 100,
        RMSMC 10 1
      ]
    benchmarks = map (uncurry3 (prepareBenchmark e)) $ filter supported xs
      where
        uncurry3 f (x, y, z) = f x y z
        xs = do
          m <- models
          s <- systems
          a <- algs
          return (s, m, a)

samplesBenchmarks :: Env -> [(Double, Bool)] -> [Double] -> [[String]] -> [Benchmark]
samplesBenchmarks e lrData hmmData ldaData = benchmarks
  where
    lrLengths = [50]
    hmmLengths = [20]
    ldaLengths = [10]
    models =
      map (LR . (`take` lrData)) lrLengths
        ++ map (HMM . (`take` hmmData)) hmmLengths
        ++ map (\n -> LDA $ map (take n) ldaData) ldaLengths
    algs =
      map (\x -> MH (100 * x)) [1 .. 10] ++ map (\x -> SMC (100 * x)) [1 .. 10]
        ++ map (\x -> RMSMC 10 (10 * x)) [1 .. 10]
    benchmarks = map (uncurry3 (prepareBenchmark e)) $ filter supported xs
      where
        uncurry3 f (x, y, z) = f x y z
        xs = do
          a <- algs
          s <- systems
          m <- models
          return (s, m, a)

main :: IO ()
main = do
  g <- stToIO (newSTGenM (mkStdGen 42))
  let e = Env g
  lrData <- sampleIOwith (LogReg.syntheticData 1000) g
  hmmData <- sampleIOwith (HMM.syntheticData 1000) g
  ldaData <- sampleIOwith (LDA.syntheticData 5 1000) g
  let configLength = defaultConfig {csvFile = Just "speed-length.csv", rawDataFile = Just "raw.dat"}
  defaultMainWith configLength (lengthBenchmarks e lrData hmmData ldaData)
  let configSamples = defaultConfig {csvFile = Just "speed-samples.csv", rawDataFile = Just "raw.dat"}
  defaultMainWith configSamples (samplesBenchmarks e lrData hmmData ldaData)
