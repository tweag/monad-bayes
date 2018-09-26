import Criterion.Main
import Criterion.Types
import System.Random.MWC (createSystemRandom, GenIO)
import Control.Monad (replicateM)

import Debug.Trace

import GHC.IO.Handle
import System.Exit
import System.Process hiding (env)

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

-- | Path to the Anglican project with benchmarks.
anglicanPath :: String
anglicanPath = "/scratch/ams240/repos/anglican-white-paper/experiments"

-- | Running Leiningen repl process.
-- Leiningen takes a lot of time to start so we keep a repl
-- running to speed up benchmarking.
-- Contains in order input handle, output handle, and process handle.
data LeinProc = LeinProc Handle Handle ProcessHandle

anglicanModelName :: Model -> String
anglicanModelName (LR _) = "logisticRegression"
anglicanModelName (HMM _) = "hmm"
anglicanModelName (LDA _) = "lda"

clojureBool :: Bool -> String
clojureBool False = "false"
clojureBool True = "true"

-- | Format Haskell [Bool] as a Clojure Boolean vector.
clojureBoolVector :: [Bool] -> String
clojureBoolVector = clojureVector . map clojureBool

-- | Format a Haskell list as a Clojure vector.
clojureVector :: [String] -> String
clojureVector xs = "[" ++ unwords xs ++ "]"

-- | Format a Haskell list as a Clojure vector.
clojureShowVector :: Show a => [a] -> String
clojureShowVector xs = clojureVector $ map show xs

-- | Insert data into an Anglican model.
anglicanData :: LeinProc -> Model -> IO ()
anglicanData lein model = do
  anglican lein ["(use 'nstools.ns)\n"]
  anglican lein ["(ns+ " ++ anglicanModelName model ++ ")\n"]
  case model of
    LR dataset -> do
      let (xs, labels) = unzip dataset
      anglican lein ["(ns-unmap *ns* 'xs)\n"]
      anglican lein ["(def xs " ++ clojureShowVector xs ++ ")\n", "nil\n"]
      anglican lein ["(ns-unmap *ns* 'labels)\n"]
      anglican lein ["(def labels " ++ clojureBoolVector labels ++ ")\n", "nil\n"]
    HMM observations -> do
      anglican lein ["(ns-unmap *ns* 'observations)\n"]
      anglican lein ["(def observations " ++ clojureShowVector observations ++ ")\n", "nil\n"]
    LDA docs -> do
      anglican lein ["(ns-unmap *ns* 'docs)\n"]
      anglican lein ["(def docs " ++ clojureVector (map clojureShowVector docs) ++ ")\n", "nil\n"]
  anglican lein ["(use 'nstools.ns)\n"]
  anglican lein ["(ns+ anglican.core)\n"]

anglican :: LeinProc -> [String] -> IO ()
anglican (LeinProc input output _) cmds = do
  -- execute command
  mapM (hPutStr input) cmds
  hFlush input
  -- wait until all samples are produced
  waitForAnglican output

-- | Wait until an Anglican program finishes
waitForAnglican :: Handle -> IO ()
waitForAnglican handle = run where
  run = do
    l <- hGetLine handle
    -- traceIO l
    -- We recognize that Anglican is finished when the repl emits a line "nil"
    if l == "nil" then
      return ()
    else
      run

-- | Start a Leiningen process in a directory that contains benchmarks.
startLein :: IO LeinProc
startLein = do
  let setup = (shell "lein repl"){cwd = Just anglicanPath, std_in = CreatePipe, std_out = CreatePipe}
  (Just input, Just output, _, process) <- createProcess setup
  -- wait until Leiningen starts producing output to make sure it's ready
  _ <- hWaitForInput output (-1) -- wait for output indefinitely
  let lein = LeinProc input output process
  anglican lein ["(use 'nstools.ns)\n"]
  return lein


-- | Path to the WebPPL project with benchmarks.
webpplPath :: String
webpplPath = "/scratch/ams240/repos/anglican-white-paper/experiments/WebPPL"

-- | Format Haskell list as a Javascript list.
javascriptList :: Show a => [a] -> String
javascriptList xs = unwords (map show xs)

webpplModelName :: Model -> String
webpplModelName (LR _) = "logisticRegression"
webpplModelName (HMM _) = "hmm"
webpplModelName (LDA _) = "lda"

-- | Environment to execute benchmarks in.
data Env = Env {rng :: GenIO, lein :: LeinProc}

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

prepareBenchmarkable :: GenIO -> ProbProgSys -> Model -> Alg -> Benchmarkable
prepareBenchmarkable g MonadBayes model alg = nfIO $ sampleIOwith (runAlg model alg) g
prepareBenchmarkable _ Anglican _ _  = error "Anglican benchmarks not available"
prepareBenchmarkable _ WebPPL _ _ = error "WebPPL benchmarks not available"

prepareBenchmark :: Env -> ProbProgSys -> Model -> Alg -> Benchmark
prepareBenchmark e MonadBayes model alg =
  bench (show MonadBayes ++ sep ++ show model ++ sep ++ show alg) $
  prepareBenchmarkable (rng e) MonadBayes model alg where
    sep = "_"
prepareBenchmark e Anglican model alg = env prepareData (const $ bench name $ whnfIO collect) where
  name = show Anglican ++ sep ++ show model ++ sep ++ show alg
  sep = "_"
  algString (MH n) = "-a lmh -n " ++ show n
  algString (SMC n) = "-a smc -n " ++ show n
  algString (RMSMC _ _) = error "Anglican does not support resample-move SMC"
  prepareData = anglicanData (lein e) model
  collect = do
    anglican (lein e) $ ["(time (m! " ++ anglicanModelName model ++ " " ++ algString alg ++ "))\n"]
prepareBenchmark _ WebPPL model alg = bench name $ whnfIO run where
  name = show WebPPL ++ sep ++ show model ++ sep ++ show alg
  sep = "_"
  algString (MH n) = "--alg MCMC --samples " ++ show n ++ " --rejuv 0 "
  algString (SMC n) = "--alg SMC --samples " ++ show n ++ " --rejuv 0 "
  algString (RMSMC n t) = "--alg SMC --samples " ++ show n ++ " --rejuv " ++ show t ++ " "
  dataString (LR dataset) = let (xs, labels) = unzip dataset in "--xs='" ++ javascriptList xs ++ "' --labels='" ++ javascriptList (map (\b -> if b then 1 else 0) labels) ++ "'"
  dataString (HMM obs) = "--obs='" ++ javascriptList obs ++ "'"
  dataString (LDA docs) = unwords $ map (\(i,doc) -> "--doc" ++ show i ++ "='" ++ unwords doc ++ "'") (zip [1..5] docs)
  run = do
    let command = "node " ++ webpplModelName model ++ ".js " ++ algString alg ++ dataString model
    (_, _, _, process) <- createProcess $ (shell command){cwd = Just webpplPath, std_out = NoStream, std_err = NoStream}
    exitCode <- waitForProcess process
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure i -> error $ "WebPPL terminated with exit code " ++ show i


-- | Checks if the requested benchmark is implemented.
supported :: (ProbProgSys, Model, Alg) -> Bool
supported (Anglican, _, RMSMC _ _) = False
supported _ = True

systems = [
            MonadBayes
            -- Anglican,
            -- WebPPL
          ]

lengthBenchmarks e lrData hmmData ldaData = benchmarks where
  lrLengths = [10] ++ map (*100) [1..10]
  hmmLengths = [10] ++ map (*100) [1..10]
  ldaLengths = [5] ++ map (*50) [1..10]
  models =
    map (LR . (`take` lrData)) lrLengths ++
    map (HMM . (`take` hmmData)) hmmLengths ++
    map (\n -> LDA $ map (take n) ldaData) ldaLengths
  algs = [
    MH 100,
    SMC 100,
    RMSMC 10 1
    ]
  benchmarks = map (uncurry3 (prepareBenchmark e)) $ filter supported xs where
        uncurry3 f (x,y,z) = f x y z
        xs = do
          m <- models
          s <- systems
          a <- algs
          return (s,m,a)

samplesBenchmarks e lrData hmmData ldaData = benchmarks where
  lrLengths = [50]
  hmmLengths = [20]
  ldaLengths = [10]
  models = map (LR . (`take` lrData)) lrLengths ++
           map (HMM . (`take` hmmData)) hmmLengths ++
           map (\n -> LDA $ map (take n) ldaData) ldaLengths
  algs =  map (\x -> MH (100*x)) [1..10] ++ map (\x -> SMC (100*x)) [1..10]
          ++ map (\x -> RMSMC 10 (10*x)) [1..10]
  benchmarks = map (uncurry3 (prepareBenchmark e)) $ filter supported xs where
        uncurry3 f (x,y,z) = f x y z
        xs = do
          a <- algs
          s <- systems
          m <- models
          return (s,m,a)

main :: IO ()
main = do

  g <- createSystemRandom
  l <- startLein
  let e = Env g l

  lrData <- sampleIOwith (LogReg.syntheticData 1000) g
  hmmData <- sampleIOwith (HMM.syntheticData 1000) g
  ldaData <- sampleIOwith (LDA.syntheticData 5 1000) g

  let configLength = defaultConfig{csvFile = Just "speed-length.csv", rawDataFile = Just "raw.dat"}
  defaultMainWith configLength (lengthBenchmarks e lrData hmmData ldaData)

  let configSamples = defaultConfig{csvFile = Just "speed-samples.csv", rawDataFile = Just "raw.dat"}
  defaultMainWith configSamples (samplesBenchmarks e lrData hmmData ldaData)
