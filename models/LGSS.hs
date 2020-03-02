{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (replicateM, unless, when)
import Control.Monad.Bayes.Inference
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Prior
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Simple hiding (normalize)
import Control.Monad.Trans
import Data.Bifunctor (second)
import Data.List (transpose)
import Data.Semigroup ((<>))
import Data.Vector (Vector, foldM, fromList, postscanl)
import qualified Data.Vector as Vector
import Graphics.Rendering.Chart.Backend.Cairo (FileFormat (PDF), fo_format, toFile)
import Graphics.Rendering.Chart.Easy hiding (Vector)
import Options.Applicative
import Plotting
import System.Directory
import System.IO

main = do
  -- make sure `putStrLn` prints to console immediately
  hSetBuffering stdout LineBuffering
  (trial, cachePath) <- execParser opts
  when trial $ putStrLn "Trial run"
  if not trial
    then sampleIO $ lgssBenchmark cachePath 50 100 (map (2 ^) [1 .. 10])
    else sampleIO $ lgssBenchmark cachePath 5 10 [10, 20, 40]

opts :: ParserInfo (Bool, FilePath)
opts = info ((,) <$> trialFlag <*> cacheDir) fullDesc
  where
    trialFlag =
      switch
        ( long "trial"
            <> help "Run a quick version of benchmarks to check that all is working correctly."
        )
    cacheDir =
      strOption
        ( long "cache-dir"
            <> value "cache/lgss/"
            <> help "Directory to store temporary data that may be reused accross different runs."
        )

lgssBenchmark :: FilePath -> Int -> Int -> [Int] -> SamplerIO ()
lgssBenchmark cachePath t nRuns ns = do
  liftIO $ putStrLn "running LGSS benchmark"
  let plotPath = "lgss.pdf"
  let param = LGSSParam (0, 1) 1 0 1 1 0 1
  scores <- replicateM nRuns $ do
    ys <- synthesizeData param t
    let ref = kalman param ys
    let run m = do
          -- RMSE on the mean of filetering distribution
          estMean <- popAvg Vector.last (normalize m)
          let trueMean = fst (Vector.last ref)
          return $ abs (trueMean - estMean)
    mapM (\n -> run $ smcMultinomial t n (linearGaussian param ys)) ns
  let points = zip (map fromIntegral ns) (transpose scores)
  liftIO $ toFile (fo_format .~ PDF $ def) plotPath $ do
    layout_title .= "LGSS"
    errorbarPlot
      "#samples"
      "RMSE"
      [ ("SMC", points)
      ]

type Mean = Double

type StdDev = Double

type Linear = (Double, Double)

data LGSSParam
  = LGSSParam
      { -- | initial state X0 prior
        p0 :: (Mean, StdDev),
        -- | transition model slope
        a :: Double,
        -- | transition model intercept
        b :: Double,
        -- | transition model noise
        sdX :: StdDev,
        -- | observation model slope
        c :: Double,
        -- | observation model intercept
        d :: Double,
        -- | observation model noise
        sdY :: StdDev
      }

-- \ One-dimensional linear Gaussian state space model.
linearGaussian ::
  (MonadBayes m, CustomReal m ~ Double) =>
  LGSSParam ->
  -- | observed sequence Y_{1:T}
  Vector Double ->
  -- | latent sequence posterior X_{1:T}
  m (Vector Double)
linearGaussian (LGSSParam p0 a b sdX c d sdY) ys = do
  let step xs y = do
        x' <- normal (a * head xs + b) sdX
        observe (normalDist (c * x' + d) sdY) y
        return (x' : xs)
  x0 <- uncurry normal p0
  ps <- foldM step [x0] ys
  return $ fromList $ tail $ reverse ps

-- | One-dimensional random walk with Gaussian diffusion.
randomWalk ::
  (MonadBayes m, CustomReal m ~ Double) =>
  -- | initial state X0 prior
  (Mean, StdDev) ->
  -- | transition model noise
  StdDev ->
  -- | observation model noise
  StdDev ->
  -- | observed sequence Y_{1:T}
  Vector Double ->
  m (Vector Double)
randomWalk p0 sdX sdY = linearGaussian (LGSSParam p0 1 0 sdX 1 0 sdY)

-- | Generate observed sequence from the prior.
synthesizeData ::
  (MonadDist m, CustomReal m ~ Double) =>
  LGSSParam ->
  -- | T - length of producted vector
  Int ->
  -- | data Y_{1:T} generated from the prior
  m (Vector Double)
synthesizeData param t = do
  xs <- prior $ linearGaussian param $ Vector.replicate t undefined
  Vector.mapM (\x -> normal (c param * x + d param) (sdY param)) xs

-- | Kalman filter computing exact filtering distributions X_t|Y_{1:t}.
kalman ::
  LGSSParam ->
  Vector Double ->
  Vector (Mean, StdDev)
kalman (LGSSParam (m0, sd0) a b sdX c d sdY) ys =
  Vector.map (second sqrt) $ postscanl step (m0, var0) ys
  where
    var0 = sd0 * sd0
    varX = sdX * sdX
    varY = sdY * sdY
    step (m, s) y = (m'', s'')
      where
        -- TODO: check the math here
        m' = a * m + b
        s' = a * a * s + varX
        v = y - c * m' - d
        h = c * c * s' + varY
        k = c * s' / h
        m'' = m' + k * v
        s'' = s' - k * k * h
