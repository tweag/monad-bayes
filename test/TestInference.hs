{-# LANGUAGE
  TupleSections
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

sprinkler :: MonadBayes m => m Bool
sprinkler = Sprinkler.soft

g = mkStdGen 0

check_terminate_smc = stdSample (smc' 2 5 sprinkler) g

check_preserve_smc = (enumerate . runIdentityT . transform . smc 2 2) sprinkler ~==
                      enumerate sprinkler

sprinkler_posterior' :: TraceT [Cache] (WeightedT Dist) Bool
sprinkler_posterior' = mapMonad duplicateWeight sprinkler
sprinkler_posterior = duplicateWeight sprinkler

mhPriorTrans :: MonadDist m => WeightedT m Bool -> m Bool
mhPriorTrans d = fmap (!! 1) $ mh 2 d (MHKernel $ const $ fmap (,1) sprinkler)

check_prior_trans = enumerate (mhPriorTrans sprinkler_posterior) ~==
                    enumerate sprinkler

pimhTrans :: MonadDist m => WeightedT m Bool -> m Bool
pimhTrans d = fmap (!! 1) $ mh 2 d kernel where
  kernel = MHKernel $ const $ fmap (,1) $ transform $ smc 2 2 sprinkler

check_pimh_trans = enumerate (pimhTrans sprinkler_posterior) ~==
                    enumerate sprinkler

mhTraceTrans :: TraceT [Cache] (WeightedT Dist) Bool -> Dist Bool
mhTraceTrans d = fmap ((!! 1) . map fst) $ mh 2 (runTraceT d) kernel where
  kernel = mhKernel' empty sprinkler

check_trace_trans = enumerate (mhTraceTrans sprinkler_posterior') ~==
                    enumerate sprinkler
