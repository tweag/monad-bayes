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
  )
where

import Control.Monad.Bayes.Class (MonadInfer)
import Control.Monad.Bayes.Inference.SMC (smcSystematic)
import Control.Monad.Bayes.Population as Pop
  ( Population,
    hoist,
    pushEvidence,
    runPopulation,
  )
import Control.Monad.Bayes.Sequential (Sequential)
import Control.Monad.Bayes.Traced.Static (Traced, mh)
import Control.Monad.Trans (lift)
import Numeric.Log (Log)

-- | Particle Marginal Metropolis-Hastings sampling.
pmmh ::
  (MonadInfer n m, RealFloat n) =>
  -- | number of Metropolis-Hastings steps
  Int ->
  -- | number of time steps
  Int ->
  -- | number of particles
  Int ->
  -- | model parameters prior
  Traced m n b ->
  -- | model
  (b -> Sequential (Population m) n a) ->
  m n [[(a, Log n)]]
pmmh t k n param model =
  mh t (param >>= runPopulation . pushEvidence . Pop.hoist undefined . smcSystematic k n . model)
