{-# LANGUAGE
  TupleSections,
  Rank2Types,
  TypeFamilies
 #-}

module TestInference where

import Data.AEq
import Control.Monad.Trans.Identity

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted hiding (prior)
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Inference
import Sprinkler
import qualified StrictlySmallerSupport

sprinkler :: MonadInfer m => m Bool
sprinkler = Sprinkler.soft

-- | Count the number of particles produced by SMC
checkParticles :: Int -> Int -> IO Int
checkParticles observations particles =
  sampleIOfixed (fmap length (runPopulation $ smcMultinomial observations particles Sprinkler.soft))

checkTerminateSMC :: IO [(Bool, Double)]
checkTerminateSMC = sampleIOfixed (smcMultinomial' 2 5 sprinkler)

checkPreserveSMC :: Bool
checkPreserveSMC = (enumerate . collapse . smcMultinomial 2 2) sprinkler ~==
                      enumerate sprinkler

-- checkPreserve_ismh = (enumerate . collapse . ismh 1 2) sprinkler ~==
--                       enumerate sprinkler
--
-- checkPreserveSmh = (enumerate . collapse . smh 1 2) sprinkler ~==
--                       enumerate sprinkler
--
-- checkPreserveSMCrm = (enumerate . collapse . SMCrm 1 2 1) sprinkler ~==
--                         enumerate sprinkler

-- sprinkler_posterior :: MonadInfer m => Weighted m Bool
-- sprinkler_posterior = duplicateWeight sprinkler

-- mhPriorrans :: MonadDist m => Weighted m Bool -> m Bool
-- mhPriorrans d = fmap (!! 1) $ mh 2 d (MHKernel $ const $ fmap (,1) sprinkler)

-- checkPriorTrans = enumerate (fmap (!! 2) (mhPrior 2 sprinkler_posterior)) ~==
--                     enumerate sprinkler

-- pimhTrans :: MonadDist m => Weighted m Bool -> m Bool
-- pimhTrans d = fmap (!! 1) $ mh 2 d kernel where
--   kernel = MHKernel $ const $ fmap (,1) $ collapse $ SMC 2 2 sprinkler

-- checkPimhTrans = enumerate (fmap (!! 2) (pimh 2 2 2 sprinkler_posterior)) ~==
--                    enumerate sprinkler

-- checkTrace_mh m m' = enumerate (dropTrace (mhStep (mhStep m))) ~==
--                       enumerate m'
--
-- trace_mh_length n = fmap length (sampleIOfixed (traceMH n sprinkler))
--
-- checkTraceTrans = checkTrace_mh sprinkler sprinkler
--
-- checkTraceSupport = checkTrace_mh StrictlySmallerSupport.model StrictlySmallerSupport.model

-- custom_mhTest :: (MHKernel k, KernelDomain k ~ Trace Double, MHSampler k ~ Dist.Enumerator (Deterministic Double)) => k -> [(Bool, Double)]
-- custom_mhTest k = enumerate (s >>= \x -> (fmap (!! 0) (runMCMC (mh sprinkler k) 1 x)))  where
--   s = marginal $ joint sprinkler
--
-- custom_mhTarget = enumerate sprinkler
--
-- check_custom_mh =
--   enumerate sprinkler ~==
--     custom_mhTest (singleSiteTraceKernel 0 identityKernel (discreteKernel (replicate 2 [0.5,0.5])))
--
-- checkPrior_mh =
--   enumerate sprinkler ~==
--     custom_mhTest (mhPriorKernel $ prior $ sprinkler >> return ())
