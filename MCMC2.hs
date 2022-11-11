{-# LANGUAGE
  ImportQualifiedPost
, FlexibleContexts
, BlockArguments
, TupleSections
, FlexibleContexts
, OverloadedStrings
, LambdaCase
, RankNTypes
, ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Control.Monad.Bayes.Class hiding (posteriorPredictive, Histogram)
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.Bayes.Traced.Static

import qualified Debug.Trace as D
import qualified Data.ByteString.Lazy as BL
import Data.Csv.Incremental (encodeRecord, encode)

import System.FilePath ()


main :: IO ()
main = do print trueSigma
          print truePosteriorMu
          is <- mhRunSingleObs
          let ss = zip ((map show [1 :: Int ..]) :: [String]) is
          BL.writeFile "SingleObs.csv" $ encode $
            foldMap encodeRecord ss

mu0, sigma0, sigma, z :: Floating a => a
mu0 = 0.0
sigma0 = 1.0
sigma = 0.5
z = 4.0

truePosteriorMu :: Double
truePosteriorMu = z * sigma0^2 / (sigma^2 + sigma0^2) +
                  mu0 * sigma^2 / (sigma^2 + sigma0^2)

trueSigma :: Double
trueSigma = sqrt $ recip (recip sigma0^2 + recip sigma^2)

singleObs :: (MonadDistribution m, MonadFactor m) => m Double
singleObs = do
    mu <- normal mu0 sigma0
    factor $ normalPdf mu sigma z
    D.trace ("singleObs: " ++ show mu) $ return ()
    return mu

mhRunSingleObs :: IO [Double]
mhRunSingleObs = do
  let nSamples = 2
  ts <- sampleIOfixed $ unweighted $ mh nSamples singleObs
  return ts


