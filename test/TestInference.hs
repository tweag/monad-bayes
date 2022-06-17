{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module TestInference where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler
import Data.AEq
import Numeric.Log
import System.Random.Stateful
import Sprinkler

sprinkler :: MonadInfer m => m Bool
sprinkler = Sprinkler.soft

-- | Count the number of particles produced by SMC
checkParticles :: Int -> Int -> IO Int
checkParticles observations particles =
  newIOGenM (mkStdGen 1729) >>=
  sampleIOwith (fmap length (runPopulation $ smcMultinomial observations particles Sprinkler.soft))

checkParticlesSystematic :: Int -> Int -> IO Int
checkParticlesSystematic observations particles =
  newIOGenM (mkStdGen 1729) >>=
  sampleIOwith (fmap length (runPopulation $ smcSystematic observations particles Sprinkler.soft))

checkParticlesStratified :: Int -> Int -> IO Int
checkParticlesStratified observations particles =
  newIOGenM (mkStdGen 1729) >>=
  sampleIOwith (fmap length (runPopulation $ smcStratified observations particles Sprinkler.soft))

checkTerminateSMC :: IO [(Bool, Log Double)]
checkTerminateSMC = newIOGenM (mkStdGen 1729) >>=
                    sampleIOwith (runPopulation $ smcMultinomial 2 5 sprinkler)

checkPreserveSMC :: Bool
checkPreserveSMC =
  (enumerate . collapse . smcMultinomial 2 2) sprinkler
    ~== enumerate sprinkler
