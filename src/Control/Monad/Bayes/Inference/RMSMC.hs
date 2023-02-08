{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Control.Monad.Bayes.Inference.RMSMC
-- Description : Resample-Move Sequential Monte Carlo (RM-SMC)
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- Resample-move Sequential Monte Carlo (RM-SMC) sampling.
--
-- Walter Gilks and Carlo Berzuini. 2001. Following a moving target - Monte Carlo inference for dynamic Bayesian models. /Journal of the Royal Statistical Society/ 63 (2001), 127-146. <http://www.mathcs.emory.edu/~whalen/Papers/BNs/MonteCarlo-DBNs.pdf>
module Control.Monad.Bayes.Inference.RMSMC
  ( rmsmc,
    rmsmcDynamic,
    rmsmcBasic,
  )
where

import Control.Monad.Bayes.Class (MonadDistribution)
import Control.Monad.Bayes.Inference.MCMC (MCMCConfig (..))
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Population
  ( PopulationT,
    flatten,
    withParticles,
  )
import Control.Monad.Bayes.Sequential.Coroutine as Seq
import Control.Monad.Bayes.Sequential.Coroutine qualified as S
import Control.Monad.Bayes.Traced.Basic qualified as TrBas
import Control.Monad.Bayes.Traced.Dynamic qualified as TrDyn
import Control.Monad.Bayes.Traced.Static as Tr
  ( TracedT,
    marginal,
    mhStep,
  )
import Control.Monad.Bayes.Traced.Static qualified as TrStat
import Data.Monoid (Endo (..))

-- | Resample-move Sequential Monte Carlo.
rmsmc ::
  (MonadDistribution m) =>
  MCMCConfig ->
  SMCConfig m ->
  -- | model
  SequentialT (TracedT (PopulationT m)) a ->
  PopulationT m a
rmsmc (MCMCConfig {..}) (SMCConfig {..}) =
  marginal
    . S.sequentially (composeCopies numMCMCSteps (TrStat.hoist flatten . mhStep) . TrStat.hoist resampler) numSteps
    . S.hoistFirst (TrStat.hoist (withParticles numParticles))

-- | Resample-move Sequential Monte Carlo with a more efficient
-- tracing representation.
rmsmcBasic ::
  (MonadDistribution m) =>
  MCMCConfig ->
  SMCConfig m ->
  -- | model
  SequentialT (TrBas.TracedT (PopulationT m)) a ->
  PopulationT m a
rmsmcBasic (MCMCConfig {..}) (SMCConfig {..}) =
  TrBas.marginal
    . S.sequentially (TrBas.hoist flatten . composeCopies numMCMCSteps (TrBas.hoist flatten . TrBas.mhStep) . TrBas.hoist resampler) numSteps
    . S.hoistFirst (TrBas.hoist (withParticles numParticles))

-- | A variant of resample-move Sequential Monte Carlo
-- where only random variables since last resampling are considered
-- for rejuvenation.
rmsmcDynamic ::
  (MonadDistribution m) =>
  MCMCConfig ->
  SMCConfig m ->
  -- | model
  SequentialT (TrDyn.TracedT (PopulationT m)) a ->
  PopulationT m a
rmsmcDynamic (MCMCConfig {..}) (SMCConfig {..}) =
  TrDyn.marginal
    . S.sequentially (TrDyn.freeze . composeCopies numMCMCSteps (TrDyn.hoist flatten . TrDyn.mhStep) . TrDyn.hoist resampler) numSteps
    . S.hoistFirst (TrDyn.hoist (withParticles numParticles))

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k = withEndo (mconcat . replicate k)

withEndo :: (Endo a -> Endo b) -> (a -> a) -> b -> b
withEndo f = appEndo . f . Endo
