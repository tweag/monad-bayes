

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
    pmmhNamed
  )
where

import Control.Monad.Bayes.Class ( MonadInfer )
import Control.Monad.Bayes.Inference.SMC ( smcSystematic )
import Control.Monad.Bayes.Population as Pop
    ( hoist, pushEvidence, runPopulation, Population )
import Control.Monad.Bayes.Sequential ( Sequential )
import Control.Monad.Bayes.Traced.Static ( mh, Traced )
import Control.Monad.Trans (lift)
import qualified Control.Monad.Bayes.Traced.Named as Named
import Control.Monad.Bayes.Traced.Named (Proposal)
import Numeric.Log
import Control.Monad.State (evalStateT, StateT)
import Data.Text (Text)

-- | Particle Marginal Metropolis-Hastings sampling.
pmmh ::
  MonadInfer m =>
  -- | number of Metropolis-Hastings steps
  Int ->
  -- | number of time steps
  Int ->
  -- | number of particles
  Int ->
  -- | model parameters prior
  Traced m b ->
  -- | model
  (b -> Sequential (Population m) a) ->
  m [[(a, Log Double)]]
pmmh t k n param model =
  mh t (param >>= runPopulation . pushEvidence . Pop.hoist lift . smcSystematic k n . model)


-- | Particle Marginal Metropolis-Hastings sampling.
pmmhNamed ::
  MonadInfer m =>
  -- | number of Metropolis-Hastings steps
  Proposal m ->
  Int ->
  -- | number of time steps
  Int ->
  -- | number of particles
  Int ->
  -- | model parameters prior
  Named.Traced (StateT Text m) b ->
  -- | model
  (b -> Sequential (Population (StateT Text m)) a) ->
  m [[(a, Log Double)]]
pmmhNamed proposal t k n param model =
  flip evalStateT "" $ Named.mh proposal t (param >>= runPopulation . pushEvidence . Pop.hoist lift . smcSystematic k n . model)

