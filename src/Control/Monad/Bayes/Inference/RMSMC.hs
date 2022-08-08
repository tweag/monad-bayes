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

import Control.Monad.Bayes.Class (MonadSample)
import Control.Monad.Bayes.Inference.MCMC (MCMCConfig (..))
import Control.Monad.Bayes.Inference.SMC (SMCConfig (..))
import Control.Monad.Bayes.Population
  ( Population,
    spawn,
    withParticles,
  )
import Control.Monad.Bayes.Sequential as Seq (Sequential, sequentially)
import Control.Monad.Bayes.Sequential qualified as S
import Control.Monad.Bayes.Traced.Basic qualified as TrBas
import Control.Monad.Bayes.Traced.Dynamic qualified as TrDyn
import Control.Monad.Bayes.Traced.Static as Tr
  ( Traced,
    marginal,
    mhStep,
  )
import Control.Monad.Bayes.Traced.Static qualified as TrStat
import Data.Monoid (Endo (..))

-- | Resample-move Sequential Monte Carlo.
rmsmc ::
<<<<<<< HEAD
  (MonadSample n m, RealFloat n) =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | number of Metropolis-Hastings transitions after each resampling
  Int ->
  -- | model
  Sequential (Traced (Population m)) n a ->
  Population m n a
rmsmc k n t =
=======
  MonadSample m =>
  MCMCConfig ->
  SMCConfig m ->
  -- | model
  Sequential (Traced (Population m)) a ->
  Population m a
rmsmc (MCMCConfig {..}) (SMCConfig {..}) =
>>>>>>> api
  marginal
    . sequentially (composeCopies numMCMCSteps mhStep . TrStat.hoist resampler) numSteps
    . S.hoistFirst (TrStat.hoist (spawn numParticles >>))

-- | Resample-move Sequential Monte Carlo with a more efficient
-- tracing representation.
rmsmcBasic ::
<<<<<<< HEAD
  (MonadSample n m, RealFloat n) =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | number of Metropolis-Hastings transitions after each resampling
  Int ->
  -- | model
  Sequential (TrBas.Traced (Population m)) n a ->
  Population m n a
rmsmcBasic k n t =
=======
  MonadSample m =>
  MCMCConfig ->
  SMCConfig m ->
  -- | model
  Sequential (TrBas.Traced (Population m)) a ->
  Population m a
rmsmcBasic (MCMCConfig {..}) (SMCConfig {..}) =
>>>>>>> api
  TrBas.marginal
    . sequentially (composeCopies numMCMCSteps TrBas.mhStep . TrBas.hoist resampler) numSteps
    . S.hoistFirst (TrBas.hoist (withParticles numParticles))

-- | A variant of resample-move Sequential Monte Carlo
-- where only random variables since last resampling are considered
-- for rejuvenation.
<<<<<<< HEAD
rmsmcLocal ::
  (MonadSample n m, RealFloat n) =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | number of Metropolis-Hastings transitions after each resampling
  Int ->
  -- | model
  Sequential (TrDyn.Traced (Population m)) n a ->
  Population m n a
rmsmcLocal k n t =
=======
rmsmcDynamic ::
  MonadSample m =>
  MCMCConfig ->
  SMCConfig m ->
  -- | model
  Sequential (TrDyn.Traced (Population m)) a ->
  Population m a
rmsmcDynamic (MCMCConfig {..}) (SMCConfig {..}) =
>>>>>>> api
  TrDyn.marginal
    . sequentially (TrDyn.freeze . composeCopies numMCMCSteps TrDyn.mhStep . TrDyn.hoist resampler) numSteps
    . S.hoistFirst (TrDyn.hoist (withParticles numParticles))

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k = withEndo (mconcat . replicate k)

withEndo :: (Endo a -> Endo b) -> (a -> a) -> b -> b
withEndo f = appEndo . f . Endo
