{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

module TestInference (checkParticles, checkParticlesSystematic, checkParticlesStratified, checkTerminateSMC, checkPreserveSMC) where

import Control.Monad.Bayes.Class (MonadInfer)
import Control.Monad.Bayes.Enumerator (enumerate)
import Control.Monad.Bayes.Inference.SMC
  ( smcMultinomial,
    smcStratified,
    smcSystematic,
  )
import Control.Monad.Bayes.Population (collapse, runPopulation)
import Control.Monad.Bayes.Sampler (sampleIOfixed)
import Data.AEq (AEq ((~==)))
import Numeric.Log (Log)
import Sprinkler (soft)

sprinkler :: MonadInfer m => m Bool
sprinkler = Sprinkler.soft

-- | Count the number of particles produced by SMC
checkParticles :: Int -> Int -> IO Int
checkParticles observations particles =
  sampleIOfixed (fmap length (runPopulation $ smcMultinomial observations particles Sprinkler.soft))

checkParticlesSystematic :: Int -> Int -> IO Int
checkParticlesSystematic observations particles =
  sampleIOfixed (fmap length (runPopulation $ smcSystematic observations particles Sprinkler.soft))

checkParticlesStratified :: Int -> Int -> IO Int
checkParticlesStratified observations particles =
  sampleIOfixed (fmap length (runPopulation $ smcStratified observations particles Sprinkler.soft))

checkTerminateSMC :: IO [(Bool, Log Double)]
checkTerminateSMC = sampleIOfixed (runPopulation $ smcMultinomial 2 5 sprinkler)

checkPreserveSMC :: Bool
checkPreserveSMC =
  (enumerate . collapse . smcMultinomial 2 2) sprinkler
    ~== enumerate sprinkler
