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

data Proposal = SingleSiteMH

data MCMCConfig = MCMCConfig {proposal :: Proposal, numMCMCSteps :: Int, numBurnIn :: Int}

defaultMCMCConfig :: MCMCConfig
defaultMCMCConfig = MCMCConfig {proposal = SingleSiteMH, numMCMCSteps = 1, numBurnIn = 0}

mcmc :: MonadSample m => MCMCConfig -> Static.Traced m a -> m [a]
mcmc (MCMCConfig {..}) m = burnIn numBurnIn $ Static.mh numMCMCSteps m

mcmcBasic :: MonadSample m => MCMCConfig -> Basic.Traced m a -> m [a]
mcmcBasic (MCMCConfig {..}) m = burnIn numBurnIn $ Basic.mh numMCMCSteps m

mcmcDynamic :: MonadSample m => MCMCConfig -> Dynamic.Traced m a -> m [a]
mcmcDynamic (MCMCConfig {..}) m = burnIn numBurnIn $ Dynamic.mh numMCMCSteps m
