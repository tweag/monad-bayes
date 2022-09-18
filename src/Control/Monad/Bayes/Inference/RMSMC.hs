{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Control.Monad.Bayes.Inference.RMSMC
-- Description : Resample-Move SequentialT Monte Carlo (RM-SMC)
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- Resample-move SequentialT Monte Carlo (RM-SMC) sampling.
--
-- Walter Gilks and Carlo Berzuini. 2001. Following a moving target - Monte Carlo inference for dynamic Bayesian models. /Journal of the Royal Statistical Society/ 63 (2001), 127-146. <http://www.mathcs.emory.edu/~whalen/Papers/BNs/MonteCarlo-DBNs.pdf>
module Control.Monad.Bayes.Inference.RMSMC
  ( rmsmc,
    rmsmcDynamic,
    rmsmcBasic,
  )
where

import Control.Monad.Bayes.Class (MonadSample)
import Control.Monad.Bayes.Inference.MCMC (MCMCConfig (..))
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Population
  ( Population,
    spawn,
    withParticles,
  )
import Control.Monad.Bayes.SequentialT as Seq (SequentialT, sequentially)
import Control.Monad.Bayes.SequentialT qualified as S
import Control.Monad.Bayes.TracedT.Basic qualified as TrBas
import Control.Monad.Bayes.TracedT.Dynamic qualified as TrDyn
import Control.Monad.Bayes.TracedT.Static as Tr
  ( TracedT,
    marginal,
    mhStep,
  )
import Control.Monad.Bayes.TracedT.Static qualified as TrStat
import Data.Monoid (Endo (..))

-- | Resample-move SequentialT Monte Carlo.
rmsmc ::
  MonadSample m =>
  MCMCConfig ->
  SMCConfig m ->
  -- | model
  SequentialT (TracedT (Population m)) a ->
  Population m a
rmsmc (MCMCConfig {..}) (SMCConfig {..}) =
  marginal
    . sequentially (composeCopies numMCMCSteps mhStep . TrStat.hoist resampler) numSteps
    . S.hoistFirst (TrStat.hoist (spawn numParticles >>))

-- | Resample-move SequentialT Monte Carlo with a more efficient
-- tracing representation.
rmsmcBasic ::
  MonadSample m =>
  MCMCConfig ->
  SMCConfig m ->
  -- | model
  SequentialT (TrBas.TracedT (Population m)) a ->
  Population m a
rmsmcBasic (MCMCConfig {..}) (SMCConfig {..}) =
  TrBas.marginal
    . sequentially (composeCopies numMCMCSteps TrBas.mhStep . TrBas.hoist resampler) numSteps
    . S.hoistFirst (TrBas.hoist (withParticles numParticles))

-- | A variant of resample-move SequentialT Monte Carlo
-- where only random variables since last resampling are considered
-- for rejuvenation.
rmsmcDynamic ::
  MonadSample m =>
  MCMCConfig ->
  SMCConfig m ->
  -- | model
  SequentialT (TrDyn.TracedT (Population m)) a ->
  Population m a
rmsmcDynamic (MCMCConfig {..}) (SMCConfig {..}) =
  TrDyn.marginal
    . sequentially (TrDyn.freeze . composeCopies numMCMCSteps TrDyn.mhStep . TrDyn.hoist resampler) numSteps
    . S.hoistFirst (TrDyn.hoist (withParticles numParticles))

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k = withEndo (mconcat . replicate k)

withEndo :: (Endo a -> Endo b) -> (a -> a) -> b -> b
withEndo f = appEndo . f . Endo
