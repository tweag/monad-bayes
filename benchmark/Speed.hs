{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.Bayes.Class (MonadInfer, MonadSample)
import Control.Monad.Bayes.Inference.MCMC (MCMCConfig (MCMCConfig, numBurnIn, numMCMCSteps, proposal), Proposal (SingleSiteMH))
import Control.Monad.Bayes.Inference.RMSMC (rmsmcDynamic)
import Control.Monad.Bayes.Inference.SMC (SMCConfig (SMCConfig, numParticles, numSteps, resampler), smc)
import Control.Monad.Bayes.Population (population, resampleSystematic)
import Control.Monad.Bayes.Sampler.Strict (SamplerIO, sampleIOfixed, sampleIOwith)
import Control.Monad.Bayes.Traced (mh)
import Control.Monad.Bayes.Weighted (unweighted)
import Criterion.Main
  ( Benchmark,
    Benchmarkable,
    bench,
    defaultConfig,
    defaultMainWith,
    nfIO,
  )
import Criterion.Types (Config (csvFile, rawDataFile))
import Data.Functor (void)
import Data.Text qualified as T
import HMM qualified
import LDA qualified
import LogReg qualified
import System.Process.Typed (runProcess)
import System.Random.Stateful (IOGenM, StatefulGen, StdGen, mkStdGen, newIOGenM)

data ProbProgSys = MonadBayes
  deriving stock (Show)

data Model = LR [(Double, Bool)] | HMM [Double] | LDA [[T.Text]]

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
runAlg model (MH n) = show <$> unweighted (mh n (buildModel model))
runAlg model (SMC n) = show <$> population (smc SMCConfig {numSteps = (modelLength model), numParticles = n, resampler = resampleSystematic} (buildModel model))
runAlg model (RMSMC n t) =
  show
    <$> population
      ( rmsmcDynamic
          MCMCConfig {numMCMCSteps = t, numBurnIn = 0, proposal = SingleSiteMH}
          SMCConfig {numSteps = modelLength model, numParticles = n, resampler = resampleSystematic}
          (buildModel model)
      )

prepareBenchmarkable :: ProbProgSys -> Model -> Alg -> Benchmarkable
prepareBenchmarkable MonadBayes model alg = nfIO $ sampleIOfixed (runAlg model alg)

prepareBenchmark :: ProbProgSys -> Model -> Alg -> Benchmark
prepareBenchmark MonadBayes model alg =
  bench (show MonadBayes ++ sep ++ show model ++ sep ++ show alg) $
    prepareBenchmarkable MonadBayes model alg
  where
    sep = "_" :: String

-- | Checks if the requested benchmark is implemented.
supported :: (ProbProgSys, Model, Alg) -> Bool
supported (_, _, RMSMC _ _) = True
supported _ = True

systems :: [ProbProgSys]
systems =
  [ MonadBayes
  ]

lengthBenchmarks :: [(Double, Bool)] -> [Double] -> [[T.Text]] -> [Benchmark]
lengthBenchmarks lrData hmmData ldaData = benchmarks
  where
    lrLengths = 10 : map (* 100) [1 :: Int .. 10]
    hmmLengths = 10 : map (* 100) [1 :: Int .. 10]
    ldaLengths = 5 : map (* 50) [1 :: Int .. 10]
    models =
      map (LR . (`take` lrData)) lrLengths
        ++ map (HMM . (`take` hmmData)) hmmLengths
        ++ map (\n -> LDA $ map (take n) ldaData) ldaLengths
    algs =
      [ MH 100,
        SMC 100,
        RMSMC 10 1
      ]
    benchmarks = map (uncurry3 (prepareBenchmark)) $ filter supported xs
      where
        uncurry3 f (x, y, z) = f x y z
        xs = do
          m <- models
          s <- systems
          a <- algs
          return (s, m, a)

samplesBenchmarks :: [(Double, Bool)] -> [Double] -> [[T.Text]] -> [Benchmark]
samplesBenchmarks lrData hmmData ldaData = benchmarks
  where
    lrLengths = [50 :: Int]
    hmmLengths = [20 :: Int]
    ldaLengths = [10 :: Int]
    models =
      map (LR . (`take` lrData)) lrLengths
        ++ map (HMM . (`take` hmmData)) hmmLengths
        ++ map (\n -> LDA $ map (take n) ldaData) ldaLengths
    algs =
      map (\x -> MH (100 * x)) [1 .. 10] ++ map (\x -> SMC (100 * x)) [1 .. 10]
        ++ map (\x -> RMSMC 10 (10 * x)) [1 .. 10]
    benchmarks = map (uncurry3 (prepareBenchmark)) $ filter supported xs
      where
        uncurry3 f (x, y, z) = f x y z
        xs = do
          a <- algs
          s <- systems
          m <- models
          return (s, m, a)

main :: IO ()
main = do
  lrData <- sampleIOfixed (LogReg.syntheticData 1000)
  hmmData <- sampleIOfixed (HMM.syntheticData 1000)
  ldaData <- sampleIOfixed (LDA.syntheticData 5 1000)
  let configLength = defaultConfig {csvFile = Just "speed-length.csv", rawDataFile = Just "raw.dat"}
  defaultMainWith configLength (lengthBenchmarks lrData hmmData ldaData)
  let configSamples = defaultConfig {csvFile = Just "speed-samples.csv", rawDataFile = Just "raw.dat"}
  defaultMainWith configSamples (samplesBenchmarks lrData hmmData ldaData)
  void $ runProcess "python plots.py"
