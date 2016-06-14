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
import Control.Monad.Bayes.Trace.ByTime
import Control.Monad.Bayes.Inference
import Sprinkler
import qualified StrictlySmallerSupport
import qualified Gamma

sprinkler :: MonadBayes m => m Bool
sprinkler = Sprinkler.soft

g = mkStdGen 0

check_terminate_smc = stdSample (smc' 2 5 sprinkler) g

check_preserve_smc = (enumerate . runIdentityT . collapse . smc 2 2) sprinkler ~==
                      enumerate sprinkler

sprinkler_posterior' :: Trace [Cache] (Weighted Dist) Bool
sprinkler_posterior' = mapMonad duplicateWeight sprinkler
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

mhTracerans :: Trace [Cache] (Weighted Dist) Bool -> Dist Bool
mhTracerans d = fmap ((!! 1) . map fst) $ mh 2 (runTrace d) kernel where
  kernel = mhKernel' empty sprinkler

-- | Like mhTracerans, but builds the kernel from its argument
-- instead of from the sprinkler model.
mhTracerans' :: (forall m'. (MonadBayes m') => m' a) -> Dist a
mhTracerans' d = fmap (!! 1) (mh' empty 2 d)

check_trace_trans = enumerate (mhTracerans sprinkler_posterior') ~==
                    enumerate sprinkler

check_trace_support = enumerate (mhTracerans' StrictlySmallerSupport.model) ~==
                      enumerate StrictlySmallerSupport.model

-- | Count the number of particles produced by SMC
check_particles :: Int -> Int -> Int
check_particles observations particles =
  stdSample (fmap length (runPopulation $ smc observations particles Gamma.model)) g
