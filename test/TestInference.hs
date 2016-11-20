{-# LANGUAGE
  TupleSections,
  Rank2Types
 #-}

module TestInference where

import Data.AEq
import Control.Monad.Trans.Identity

import Control.Monad.Bayes.Class
import qualified Control.Monad.Bayes.Dist as Dist
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Empirical
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Inference
import Sprinkler
import qualified StrictlySmallerSupport
import qualified Gamma

sprinkler :: MonadBayes m => m Bool
sprinkler = Sprinkler.soft

enumerate :: Ord a => Dist.Dist Double a -> [(a,Double)]
enumerate = Dist.enumerate

check_terminate_smc = sampleIOfixed (smc' 2 5 sprinkler)

check_preserve_smc = (enumerate . collapse . smc 2 2) sprinkler ~==
                      enumerate sprinkler

check_preserve_ismh = (enumerate . collapse . ismh 1 2) sprinkler ~==
                      enumerate sprinkler

check_preserve_smh = (enumerate . collapse . smh 2 2) sprinkler ~==
                      enumerate sprinkler

check_preserve_smcrm = (enumerate . collapse . smcrm 1 2 1) sprinkler ~==
                        enumerate sprinkler

sprinkler_posterior :: MonadBayes m => Weighted m Bool
sprinkler_posterior = duplicateWeight sprinkler

-- mhPriorrans :: MonadDist m => Weighted m Bool -> m Bool
-- mhPriorrans d = fmap (!! 1) $ mh 2 d (MHKernel $ const $ fmap (,1) sprinkler)

-- check_prior_trans = enumerate (fmap (!! 2) (mhPrior 2 sprinkler_posterior)) ~==
--                     enumerate sprinkler

-- pimhTrans :: MonadDist m => Weighted m Bool -> m Bool
-- pimhTrans d = fmap (!! 1) $ mh 2 d kernel where
--   kernel = MHKernel $ const $ fmap (,1) $ collapse $ smc 2 2 sprinkler

-- check_pimh_trans = enumerate (fmap (!! 2) (pimh 2 2 2 sprinkler_posterior)) ~==
--                    enumerate sprinkler

check_trace_mh m m' = enumerate (marginal (mhStep (mhStep m))) ~==
                      enumerate m'

trace_mh_length n = fmap length (sampleIOfixed (traceMH n sprinkler))

check_trace_trans = check_trace_mh sprinkler sprinkler

check_trace_support = check_trace_mh StrictlySmallerSupport.model StrictlySmallerSupport.model

-- | Count the number of particles produced by SMC
check_particles :: Int -> Int -> IO Int
check_particles observations particles =
  sampleIOfixed (fmap length (runPopulation $ smc observations particles Gamma.model))
