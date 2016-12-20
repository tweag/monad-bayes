{-# LANGUAGE
  Rank2Types,
  TypeFamilies,
  TupleSections
 #-}

-- Import all models under maintenance.
-- Models not imported here will not be compiled
-- when invoking `stack bench`.
import qualified BetaBin
import qualified Dice
import qualified DPmixture
import qualified Gamma
import qualified HMM
import Plotting
import qualified Nonlinear

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Inference
import Control.Monad.Bayes.Population

import System.IO
import Control.Arrow (first, second)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)

import Statistics.Sample
import qualified Data.Vector as Vector
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

main = do
  -- make sure `putStrLn` prints to console immediately
  hSetBuffering stdout LineBuffering

  sampleIO hmmBenchmark


meanVar :: (MonadDist m, CustomReal m ~ Double) => Int -> m Double -> m (Double, Double)
meanVar n d = do
  xs <- Vector.replicateM n d
  return $ meanVariance xs

smcParams :: [Int]
smcParams = [10,20,50,100,200,500,1000]

ns :: [Int]
--ns = [10,20,50,100,200,500,1000]
ns = [10,20..1000]

smcParamsDouble :: [Double]
smcParamsDouble = map fromIntegral smcParams

smcResults :: (MonadDist m, CustomReal m ~ Double) => [m (Vector.Vector Double)]
smcResults = map (\p -> Vector.replicateM 10 $ fmap HMM.hmmKL $ explicitPopulation $ smc (length HMM.values) p HMM.hmm) smcParams

hmmBenchmark :: SamplerIO ()
hmmBenchmark = do
  isSamples <- fmap (drop 5000) $ importance 10000 HMM.hmm
  let isRes = map (\n -> HMM.hmmKL $ take n isSamples) ns
  mhSamples <- fmap (drop 5000) $ traceMH 10000 HMM.hmm
  let mhRes = map (\n -> HMM.hmmKL $ take n $ map (,1) mhSamples) ns
  mhPriorSamples <- fmap (drop 5000) $ mhPrior 10000 HMM.hmm
  let mhPriorRes = map (\n -> HMM.hmmKL $ take n $ map (,1) mhPriorSamples) ns
  pimhSamples <- pimh (length HMM.values) 100 1000 HMM.hmm
  let pimhRes = map (\n -> HMM.hmmKL $ take n $ map (,1) pimhSamples) ns
  liftIO $ toFile (fo_format .~ PDF $ def) "anytime.pdf" $ do
    layout_title .= "HMM"
    anytimePlot "#samples" "KL" ns [
      ("IS", isRes),
      ("MHtrace", mhRes),
      ("MHprior", mhPriorRes),
      ("PIMH", pimhRes)]


  smcRes <- sequence smcResults
  liftIO $ toFile (fo_format .~ PDF $ def) "smc.pdf" $ do
    layout_title .= "HMM"
    oneShotPlot "#particles" "KL" [
      ("SMC", zip smcParamsDouble (map (errBars 2) smcRes))]
