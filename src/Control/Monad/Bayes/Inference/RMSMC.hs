{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Trustworthy #-}

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
    rmsmcLocal,
    rmsmcBasic,
  )
where

import Control.Monad.Bayes.Class (MonadInfer, MonadSample)
import Control.Monad.Bayes.Population
  ( Population,
    collapse,
    resampleSystematic,
    runPopulation,
    spawn,
  )
import Control.Monad.Bayes.Population qualified as P
import Control.Monad.Bayes.Sequential as Seq (Sequential, sis)
import Control.Monad.Bayes.Sequential qualified as S
import Control.Monad.Bayes.Traced.Basic qualified as TrBas
import Control.Monad.Bayes.Traced.Dynamic qualified as TrDyn
import Control.Monad.Bayes.Traced.Static as Tr
  ( Traced,
    marginal,
    mhStep,
  )
import Control.Monad.Bayes.Traced.Static qualified as TrStat

-- | Resample-move Sequential Monte Carlo.
rmsmc ::
  MonadSample m =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | number of Metropolis-Hastings transitions after each resampling
  Int ->
  -- | model
  Sequential (Traced (Population m)) a ->
  Population m a
rmsmc k n t =
  marginal
    . sis (composeCopies t mhStep . TrStat.hoistT resampleSystematic) k
    . S.hoist (TrStat.hoistT (spawn n >>))

-- | Resample-move Sequential Monte Carlo with a more efficient
-- tracing representation.
rmsmcBasic ::
  MonadSample m =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | number of Metropolis-Hastings transitions after each resampling
  Int ->
  -- | model
  Sequential (TrBas.Traced (Population m)) a ->
  Population m a
rmsmcBasic k n t =
  TrBas.marginal
    . sis (composeCopies t TrBas.mhStep . TrBas.hoistT resampleSystematic) k
    . S.hoistFirst (TrBas.hoistT (spawn n >>))

-- | A variant of resample-move Sequential Monte Carlo
-- where only random variables since last resampling are considered
-- for rejuvenation.
rmsmcLocal ::
  MonadSample m =>
  -- | number of timesteps
  Int ->
  -- | number of particles
  Int ->
  -- | number of Metropolis-Hastings transitions after each resampling
  Int ->
  -- | model
  Sequential (TrDyn.Traced (Population m)) a ->
  Population m a
rmsmcLocal k n t =
  TrDyn.marginal
    . sis (TrDyn.freeze . composeCopies t TrDyn.mhStep . TrDyn.hoistT resampleSystematic) k
    . S.hoistFirst (TrDyn.hoistT (spawn n >>))

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)
