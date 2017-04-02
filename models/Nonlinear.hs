{-# LANGUAGE
 FlexibleContexts,
 TypeFamilies
 #-}

module Main where

import Prelude hiding (map, length, sum, transpose, zipWith, replicate)
import qualified Data.List as List

import System.IO
import Control.Monad (when, unless)
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Data.Vector hiding (reverse, mapM, (++))
import Control.Exception.Base

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Conditional
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference
import Control.Monad.Bayes.Sampler

import qualified Data.Vector as Vector
import Graphics.Rendering.Chart.Easy hiding (Vector)
import Graphics.Rendering.Chart.Backend.Cairo (toFile, fo_format, FileFormat(PDF))
import Options.Applicative
import System.Directory

import Plotting

-- | A nonlinear series model from Doucet et al. (2000)
-- "On sequential Monte Carlo sampling methods" section VI.B
model :: (MonadDist m, MonadTrans t, MonadDist (t m), r ~ CustomReal (t m),
          r ~ CustomReal m)
      => Int -- ^ number of time steps
      -> t m (Vector (r, r)) -- ^ vector of state-observation pairs from t=1
model t = do
  let sq x = x * x
      simulate 0 _ acc = return acc
      simulate n x acc = do
        let mean = 0.5 * x + 25 * x / (1 + sq x) +
                   8 * (cos (1.2 * fromIntegral n))
        x' <- lift $ normal mean 1
        y' <- normal (sq x' / 20) 1
        simulate (n-1) x' ((x',y'):acc)

  x0 <- lift $ normal 0 (sqrt 5)
  ps <- simulate t x0 []
  return $ fromList $ reverse ps

-- | Generative version of 'model', samples both x and y
generative :: MonadDist m => Int -> m (Vector (CustomReal m, CustomReal m))
generative = runIdentityT . model

-- | Generates synthetic data from the prior
synthesizeData :: MonadDist m => Int -> m (Vector (CustomReal m))
synthesizeData = fmap (map snd) . generative

-- | Posterior distribution over xs conditional on ys
posterior :: MonadBayes m
          => Vector (CustomReal m) -- ^ observations ys
          -> m (Vector (CustomReal m)) -- ^ latent states xs starting from t=1
posterior ys = fmap (map fst) $
  unsafeConditional (model (length ys)) (fromLists (toList ys,[]))

-- | Averages a weighted sample of vectors.
-- Assumes the weights are normalized.
averageVec :: Num r => [(Vector r, r)] -> Vector r
averageVec ps = List.foldr (zipWith (+)) zeros $
                List.map (\(v,w) -> map (* w) v) ps where
                  zeros = assert (List.all ((== k) . length . fst) ps) $
                          replicate k 0
                  k = length $ fst $ List.head ps

-- | The reference inference algorithm is SMC.
reference :: MonadDist m
          => Vector (CustomReal m) -- ^ ys
          -> Int -- ^ number of particles used for reference
          -> m (Vector (CustomReal m)) -- ^ mean of xs from all particles
reference ys n = fmap averageVec $ explicitPopulation $ normalize $
               smcMultinomial k n (posterior ys) where
  k = length ys

-- | Root-mean-square error
rmse :: Floating r
     => Vector r -- ^ reference values for the posterior mean
     -> Vector r -- ^ estimated posterior mean
     -> r
rmse ref xs = sum $ map (^ 2) $ zipWith (-) ref xs

-------------------------------------------------
-- benchmarking

opts :: ParserInfo (Bool,FilePath)
opts = flip info fullDesc ((,) <$> trialFlag <*> cacheDir) where
  trialFlag = switch
    ( long "trial"
    <> help "Run a quick version of benchmarks to check that all is working correctly.")
  cacheDir = strOption
    ( long "cache-dir"
    <> value "cache/nonlinear/"
    <> help "Directory to store temporary data that may be reused accross different runs.")

tryCache :: (MonadIO m, Read a, Show a) => FilePath -> m a -> m a
tryCache filepath fresh = do
  exists <- liftIO $ doesFileExist filepath
  if exists then
    liftIO $ fmap read (readFile filepath)
  else do
    value <- fresh
    liftIO $ writeFile filepath (show value)
    return value

nonlinearBenchmark :: FilePath -> Int -> Int -> [Int] -> Int -> SamplerIO ()
nonlinearBenchmark cachePath t nRuns ns nRef = do
  liftIO $ putStrLn "running Nonlinear benchmark"

  let dataPath = cachePath ++ "data.txt"
  let refPath = cachePath ++ "reference.txt"
  let scoresPath = cachePath ++ "scores.txt"
  let plotPath = "nonlinear.pdf"
  liftIO $ createDirectoryIfMissing True cachePath

  ys <- tryCache dataPath $ synthesizeData t
  ref <- tryCache refPath $ reference ys nRef
  let run m = fmap ((/ fromIntegral nRuns) . sum) $ Vector.replicateM nRuns $
              fmap (rmse ref . averageVec) $
              explicitPopulation $ normalize m
  scores <- tryCache scoresPath $
            mapM (\n -> run $ smcMultinomial t n (posterior ys)) ns

  liftIO $ toFile (fo_format .~ PDF $ def) plotPath $ do
    layout_title .= "Nonlinear"
    anytimePlot "#samples" "RMSE" ns [
      ("SMC", scores)]

main = do
  -- make sure `putStrLn` prints to console immediately
  hSetBuffering stdout LineBuffering

  (trial, cachePath) <- execParser opts
  when trial $ putStrLn "Trial run"

  if not trial then do
    sampleIO $ nonlinearBenchmark cachePath 50 10 [10,20,40] 10000
  else do
    sampleIO $ nonlinearBenchmark cachePath 5 10 [10,20,40] 100
