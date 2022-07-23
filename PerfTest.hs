{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted
import Control.Monad.Reader
import System.Random.MWC (createSystemRandom)

paramPriorRegression :: MonadSample m => m (Double, Double, Double)
paramPriorRegression = do
  slope <- normal 0 2
  intercept <- normal 0 2
  noise <- gamma 1 1 -- 4 4
  return (slope, intercept, noise)

regression :: (MonadInfer m) => [Double] -> [Double] -> m (Double, Double, Double)
regression xs ys = do
  (slope, intercept, noise) <- paramPriorRegression
  _ <- forM (zip xs ys) \(x, y) -> factor $ normalPdf (slope * x + intercept) (sqrt noise) y
  return (slope, intercept, noise)

syntheticData :: MonadSample m => Int -> m [(Double, Double)]
syntheticData n = do
  let xs = map fromIntegral [1 .. n]
      ys = map (3 *) xs
  eps <- replicateM n $ normal 0 0.1
  let zs = zipWith (+) ys eps
  return $ zip xs zs

main :: IO ()
main = do
  g <- createSystemRandom
  xys <- sampleWith (syntheticData 10) g
  let (xs, ys) = unzip xys
  print xs
  print ys
  putStrLn "\n"

  mhRunsRegression <- sampleWith (prior $ mh 100000 $ regression xs ys) g
  let ss = map (\(x, _, _) -> x) mhRunsRegression
  let is = map (\(_, y, _) -> y) mhRunsRegression
  let ns = map (\(_, _, z) -> z) mhRunsRegression
  print $ (/ 50000.0) $ Prelude.sum $ drop 50000 ss
  print $ (/ 50000.0) $ Prelude.sum $ drop 50000 is
  print $ (/ 50000.0) $ Prelude.sum $ drop 50000 ns
