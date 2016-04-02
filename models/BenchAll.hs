{-# LANGUAGE Rank2Types
 #-}

-- Import all models under maintenance.
-- Models not imported here will not be compiled
-- when invoking `stack bench`.
import qualified BetaBin
import qualified Dice
import qualified DPmixture
import qualified Gamma
import qualified HMM

-- Algorithms to benchmark
import Control.Monad.Bayes.Inference
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Trace.Debug hiding (mhRun)
import qualified Control.Monad.Bayes.Trace.ByTime as ByTime
import qualified Control.Monad.Bayes.Trace.ByDist as ByDist
import qualified Control.Monad.Bayes.Trace.ByType as ByType

-- Standard library
import Data.List (sort)
import Data.Typeable
import System.IO
import Text.Printf
-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Dist
import Control.Monad.Bayes.Metrics

mhRun = mhRunWith ByType.empty

main = do
  -- make sure `putStrLn` prints to console immediately
  hSetBuffering stdout LineBuffering
  hline
  putStrLn "MH on sum of 4 dice: KL-divergence"
  defaultKLBench "dice 4" $ Dice.dice 4
  hline

  putStrLn "MH on sum of 2 dice given it is at least 4: KL-divergence"
  defaultKLBench "dice_hard" Dice.dice_hard
  hline

  putStrLn "MH on sum of 2 dices weighted by its reciprocal"
  defaultKLBench "dice_soft" Dice.dice_soft
  hline
  putStrLn "KL-divergence of `latent 5` against `urn 5` by repeated sampling"
  klBench "latent 5 || urn 5" (sampleMany $ BetaBin.latent 5) (BetaBin.urn 5) 0 (powers 14)
  hline

  putStrLn "MH on `urn 5`: KL-divergence"
  defaultKLBench "urn 5" $ BetaBin.urn 5
  hline

  putStrLn "MH on `latent 2`: KL-divergence"
  klBench "latent 2" (mhRun $ BetaBin.latent 2) (BetaBin.urn 2) 0 (powers 14)
  hline


  putStrLn "MH on gaussians: KS-test against repeated sampling"
  ksBench "gaussians" (mhRun gaussians 0) (sampleMany gaussians 0) (powers 14)
  hline
  putStrLn "MH on gaussians: KS-test against another chain"
  ksBench "gaussians" (mhRun gaussians 0) (mhRun gaussians 3821) (powers 14)
  hline

  putStrLn "MH on varChoices: KS-test against repeated sampling"
  ksBench "varChoices" (mhRun varChoices 3821) (sampleMany varChoices 0) (powers 14)
  hline
  putStrLn "MH on varChoices: KS-test against another chain"
  ksBench "varChoices" (mhRun varChoices 0) (mhRun varChoices 3821) (powers 14)
  hline

  defaultKSBench "gamma model|exact" Gamma.model Gamma.exact
  hline
  defaultKSBench "gamma exact|exact" Gamma.exact Gamma.exact
  hline

hline :: IO ()
hline = putStrLn $ replicate 80 '-'

powers n = map (2^) [0..n]

-- run klBench with default config
defaultKLBench :: (Ord a, Typeable a) => String -> (forall m. MonadBayes m => m a) -> IO [()]
defaultKLBench name program = klBench name (mhRun program) program 0 (powers 14)

-- ms is a collection of sample sizes.
-- Run @maximum ms@ steps of MH, print KL-divergence against
-- the first k samples with k <- ms.
klBench :: (Ord a, Typeable a) =>
           String -> (Int -> Int -> [a]) -> Dist a -> Int -> [Int] -> IO [()]
klBench name sampler reference seed ms =
  let
    m  = maximum ms
    xs = (sampler seed m)
    kl k = putStrLn $ printf "%-20s KL-divergence with %6d sample = %f" name k
                    $ kullbackLeibnerTest (take k xs) reference

  in
    sequence $ map kl ms

-- run ksBench of MH against repeated sampling
defaultKSBench :: (Ord a) => String -> (forall m. MonadBayes m => m a) -> (forall m. MonadDist m => m a) -> IO [()]
defaultKSBench name program reference =
  ksBench name (mhRun program 0) (sampleMany reference 0) (powers 14)

-- 2-sample Kolmogorov-Smirnov test on 2 samplers
ksBench :: (Ord a) =>
           String -> (Int -> [a]) -> (Int -> [a]) -> [Int] -> IO [()]
ksBench name sampler1 sampler2 ms =
  let
    m  = maximum ms
    xs = sampler1 m
    ys = sampler2 m
    ks k = putStrLn $ printf "%-20s KS-test with %6d sample = %f" name k
                    $ kolmogorovSmirnovTest (take k xs) (take k ys)

  in
    sequence $ map ks ms
