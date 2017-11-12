import Criterion.Main
import Criterion.Types
import System.Random.MWC (createSystemRandom, GenIO)

import GHC.IO.Handle
import System.Exit
import System.Process hiding (env)
import Control.Monad.Par.Class (NFData)

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

-- | Wait until an Anglican program finishes
waitForAnglican :: Handle -> IO ()
waitForAnglican handle = run where
  run = do
    l <- hGetLine handle
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
  hWaitForInput output (-1) -- wait for output indefinitely
  return $ LeinProc input output process

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

prepareBenchmark :: Env -> ProbProgSys -> Model -> Alg -> Benchmark
prepareBenchmark e MonadBayes model alg =
  bench (show MonadBayes ++ sep ++ show model ++ sep ++ show alg) $
  prepareBenchmarkable (rng e) MonadBayes model alg where
    sep = "_"
prepareBenchmark e Anglican model alg = bench "Anglican" $ whnfIO collect where
  (LeinProc input output process) = (lein e)
  collect = do
    let cmdString = "(m! lda -a smc -n 1000)\n"
    hPutStr input cmdString
    hFlush input
    waitForAnglican output

main :: IO ()
main = do

  g <- createSystemRandom
  l <- startLein
  let e = Env g l

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

  -- let benchmarks = map (uncurry3 (prepareBenchmark e)) xs where
  --       uncurry3 f (x,y,z) = f x y z
  --       xs = do
  --         s <- systems
  --         m <- models
  --         a <- algs
  --         return (s,m,a)
  let benchmarks = [prepareBenchmark e Anglican undefined undefined]

  -- let config = defaultConfig{csvFile = Just "speed.csv"}
  let config = defaultConfig
  defaultMainWith config benchmarks
