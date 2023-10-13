{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- From random
import           System.Random.Stateful

-- From mwc-random
import qualified System.Random.MWC.Distributions as MWC

-- From random-fu
import Data.Random

-- From monad-bayes
import qualified Control.Monad.Bayes.Class as MonadBayes
import Control.Monad.Bayes.Sampler.Strict

import Criterion.Main
import Control.DeepSeq (NFData)
import Foreign


main :: IO ()
main = do
    stdGen <- newIOGenM =<< newStdGen
    defaultMain
        [ bgroup "dists"
            [ bgroup "StdGen" (dists (stdGen :: (IOGenM StdGen)))
            ]
        ]

dists :: StatefulGen g IO => g -> [Benchmark]
dists gen =
    [ doubleSuite gen "stdNormal" (Normal 0.0 1.0)
    ]

doubleSuite :: (Distribution d Double, StatefulGen g IO) => g -> String -> d Double -> Benchmark
doubleSuite = suite

suite :: (Storable t, Num t, Distribution d t, NFData t, StatefulGen g IO) => g -> String -> d t -> Benchmark
suite gen name var = bgroup name
    [ bench "single sample" $ nfIO $ do
        sampleFrom gen var
    , bench "single sample MWC" $ nfIO $ do
        MWC.normal 0.0 1.0 gen
    , bench "single sample monad bayes" $ nfIO $ do
        sampleWith (MonadBayes.normal 0.0 1.0) gen
    ]

