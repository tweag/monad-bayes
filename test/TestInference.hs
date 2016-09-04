{-# LANGUAGE
  TupleSections,
  Rank2Types
 #-}

module TestInference where

import Data.AEq
import Control.Monad.Trans.Identity
import System.Random

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Dist
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

g = mkStdGen 0

check_terminate_smc = stdSample (smc' 2 5 sprinkler) g

check_preserve_smc = (enumerate . collapse . smc 2 2) sprinkler ~==
                      enumerate sprinkler

check_preserve_smcrm = (enumerate . collapse . smcrm 2 1) sprinkler ~==
                        enumerate sprinkler

sprinkler_posterior = duplicateWeight sprinkler

mhPriorTrans :: MonadDist m => Weighted m Bool -> m Bool
mhPriorTrans d = fmap (!! 1) $ mh 2 d (MHKernel $ const $ fmap (,1) sprinkler)

check_prior_trans = enumerate (mhPriorTrans sprinkler_posterior) ~==
                    enumerate sprinkler

pimhTrans :: MonadDist m => Weighted m Bool -> m Bool
pimhTrans d = fmap (!! 1) $ mh 2 d kernel where
  kernel = MHKernel $ const $ fmap (,1) $ collapse $ smc 2 2 sprinkler

check_pimh_trans = enumerate (pimhTrans sprinkler_posterior) ~==
                   enumerate sprinkler

check_trace_mh m m' = enumerate (marginal' (mhStep' m)) ~==
                      enumerate m'

check_trace_trans = check_trace_mh sprinkler sprinkler

check_trace_support = check_trace_mh StrictlySmallerSupport.model StrictlySmallerSupport.model

-- | Count the number of particles produced by SMC
check_particles :: Int -> Int -> Int
check_particles observations particles =
  stdSample (fmap length (runPopulation $ smc observations particles Gamma.model)) g
