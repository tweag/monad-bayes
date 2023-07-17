{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Control.Monad.Bayes.Inference.PMMH
-- Description : Particle Marginal Metropolis-Hastings (PMMH)
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- Particle Marginal Metropolis-Hastings (PMMH) sampling.
--
-- Christophe Andrieu, Arnaud Doucet, and Roman Holenstein. 2010. Particle Markov chain Monte Carlo Methods. /Journal of the Royal Statistical Society/ 72 (2010), 269-342. <http://www.stats.ox.ac.uk/~doucet/andrieu_doucet_holenstein_PMCMC.pdf>
module Control.Monad.Bayes.Inference.PMMH
  ( pmmh,
    pmmhBayesianModel,
  )
where

import Control.Monad.Bayes.Class (Bayesian (generative), MonadDistribution, MonadMeasure, prior)
import Control.Monad.Bayes.Inference.MCMC (MCMCConfig, mcmc)
import Control.Monad.Bayes.Inference.SMC (SMCConfig (), smc)
import Control.Monad.Bayes.Population as Pop
  ( PopulationT,
    hoist,
    pushEvidence,
    runPopulationT,
  )
import Control.Monad.Bayes.Sequential.Coroutine (SequentialT)
import Control.Monad.Bayes.Traced.Static (TracedT)
import Control.Monad.Bayes.Weighted
import Control.Monad.Trans (lift)
import Numeric.Log (Log)

-- | Particle Marginal Metropolis-Hastings sampling.
pmmh ::
  (MonadDistribution m) =>
  MCMCConfig ->
  SMCConfig (WeightedT m) ->
  TracedT (WeightedT m) a1 ->
  (a1 -> SequentialT (PopulationT (WeightedT m)) a2) ->
  m [[(a2, Log Double)]]
pmmh mcmcConf smcConf param model =
  mcmc
    mcmcConf
    ( param
        >>= runPopulationT
          . pushEvidence
          . Pop.hoist lift
          . smc smcConf
          . model
    )

-- | Particle Marginal Metropolis-Hastings sampling from a Bayesian model
pmmhBayesianModel ::
  (MonadMeasure m) =>
  MCMCConfig ->
  SMCConfig (WeightedT m) ->
  (forall m'. (MonadMeasure m') => Bayesian m' a1 a2) ->
  m [[(a2, Log Double)]]
pmmhBayesianModel mcmcConf smcConf bm = pmmh mcmcConf smcConf (prior bm) (generative bm)
