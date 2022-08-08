{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Control.Monad.Bayes.Inference.MCMC
-- Description : Markov Chain Monte Carlo (MCMC)
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Inference.MCMC where

import Control.Monad.Bayes.Class (MonadSample)
import qualified Control.Monad.Bayes.Traced.Basic as Basic
import Control.Monad.Bayes.Traced.Common (burnIn)
import qualified Control.Monad.Bayes.Traced.Dynamic as Dynamic
import qualified Control.Monad.Bayes.Traced.Static as Static
import Control.Monad.Bayes.Weighted

data Proposal = SingleSiteMH

data MCMCConfig = MCMCConfig {proposal :: Proposal, numMCMCSteps :: Int, numBurnIn :: Int}

defaultMCMCConfig :: MCMCConfig
defaultMCMCConfig = MCMCConfig {proposal = SingleSiteMH, numMCMCSteps = 1, numBurnIn = 0}

mcmc :: MonadSample m => MCMCConfig -> Static.Traced (Weighted m) a -> m [a]
mcmc (MCMCConfig {..}) m = burnIn numBurnIn $ unweighted $ Static.mh numMCMCSteps m

mcmcBasic :: MonadSample m => MCMCConfig -> Basic.Traced (Weighted m) a -> m [a]
mcmcBasic (MCMCConfig {..}) m = burnIn numBurnIn $ unweighted $ Basic.mh numMCMCSteps m

mcmcDynamic :: MonadSample m => MCMCConfig -> Dynamic.Traced (Weighted m) a -> m [a]
mcmcDynamic (MCMCConfig {..}) m = burnIn numBurnIn $ unweighted $ Dynamic.mh numMCMCSteps m
