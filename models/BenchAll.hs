{-# LANGUAGE
  Rank2Types,
  TypeFamilies
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

import Statistics.Sample
import qualified Data.Vector as Vector
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

ns = [1..10]

-- \ KL divergence between two distributions.
-- The distributions should be normalized to ensure correct results.
kl :: (Real r, NumSpec r, Ord a) => Dist r a -> Dist r a -> r
kl p q = expectation f p where
  f x = log $ dp x / dq x
  dp = mass p
  dq = mass q

hmmKL :: (MonadDist m, CustomReal m ~ Double) => Population m [Int] -> m Double
hmmKL m = do
  -- obtain samples from the joint
  samples <- fmap (map (second fromLogDomain)) $ runPopulation m
  -- convert to a list of marginal samples for each time step
  let marginals = map (\i -> map (first (!! i)) samples) [1 .. (length HMM.values)]
  let exact = HMM.exactMarginals
  unless (length marginals == length exact) $ error $ "hmmKL: length mismatch " ++ show (length marginals) ++ " and " ++ show (length exact)
  -- compute the sum of all marginal KLs
  let result = sum $ zipWith kl (map categorical marginals) exact
  return result

meanVar :: (MonadDist m, CustomReal m ~ Double) => Int -> m Double -> m (Double, Double)
meanVar n d = do
  xs <- Vector.replicateM n d
  return $ meanVariance xs

smcParams :: [Int]
smcParams = [10,20,50,100,200,500,1000]

smcParamsDouble :: [Double]
smcParamsDouble = map fromIntegral smcParams

smcResults :: (MonadDist m, CustomReal m ~ Double) => [m (Vector.Vector Double)]
smcResults = map (\p -> Vector.replicateM 10 $ hmmKL $ smc (length HMM.values) p HMM.hmm) smcParams

smcrmResults :: (MonadDist m, CustomReal m ~ Double) => [m (Vector.Vector Double)]
smcrmResults = map (\p -> Vector.replicateM 10 $ hmmKL $ smcrm (length HMM.values) 10 p HMM.hmm) smcParams

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

main = do
  -- make sure `putStrLn` prints to console immediately
  hSetBuffering stdout LineBuffering

  smcRes <- sampleIO $ sequence $ smcResults
  putStrLn $ show smcRes
  smcrmRes <- sampleIO $ sequence $ smcrmResults

  toFile (fo_format .~ PDF $ def) "smc.pdf" $ do
    layout_title .= "HMM"
    oneShotPlot "#particles" "KL" [
      ("SMC", zip smcParamsDouble (map (errBars 2) smcRes)),
      ("RM-SMC", zip smcParamsDouble (map (errBars 2) smcrmRes))]


  -- pimhSamples <- pimh 10 100 1000 DPmixture.dpMemClusters
  -- let pimhResults = map ((`kl` DPmixture.posteriorClustersDist) . uniformD . (`take` pimhSamples) . (*100)) ns
  -- putStrLn $ show $ pimhResults

  -- smcSamples <- sampleIO $ sequence $ replicate 10 $ fmap (map (second fromLogDomain)) $ runPopulation $ smc 16 100 $ fmap head HMM.hmm
  -- let smcResults = map ((`kl` (head (tail HMM.exactMarginals))) . categorical . concat . (`take` smcSamples)) ns
  -- putStrLn $ show $ smcResults

  --putStrLn $ show $ Dist.explicit $ DPmixture.posteriorClustersDist

  --(xs,w) <- runWeighted HMM.hmm
  --putStrLn $ show $ xs

  --putStrLn $ show $ map Dist.explicit $ HMM.exactMarginals
