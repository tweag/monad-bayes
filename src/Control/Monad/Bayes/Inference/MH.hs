-- |
-- Module      : Control.Monad.Bayes.Inference.MH
-- Description :  Metropolis-Hastings (MH)
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Stability   : experimental
-- Portability : GHC
--
-- Metropolis-Hastings (MH) sampling.
module Control.Monad.Bayes.Inference.MH where

import Control.Monad.Bayes.Class (MonadSample (bernoulli), MonadInfer)
import qualified Pipes.Prelude as P
import Pipes.Core (Producer)
import Pipes ((>->))
import qualified Control.Monad.Bayes.Traced.Static as Static

-- | Trace MH (as defined in Control.Monad.Bayes.Traced) works for any probabilistic program, 
-- | but this comes at the cost of expressive power, where proposals are concerned.
-- | If instead you can specify an explicit proposal and scoring function, you can use explicitMH. 
-- | This explicitly runs a Markov Chain andis implemented using Pipes, which is a library
-- | for handling effectful lists lazily.

-- The Metropolis-Hastings criterion (see https://ermongroup.github.io/cs228-notes/inference/sampling/)
mhStep :: MonadSample m => (a -> m a) -> (a -> Double) -> (a -> m a)
mhStep proposal likelihood currentState = do
  proposed <- proposal currentState
  let score = likelihood proposed
      oldScore = likelihood currentState
      ratio = min 1 (score / oldScore)
  accept <- bernoulli ratio
  return (if accept then proposed else currentState)

stochastically :: MonadSample m => Int -> a -> (a -> m a) -> Producer a m ()
stochastically i initial transition = do
    P.unfoldr (fmap (Right . (\a -> (a,a))) . transition) initial
    >-> P.take i

explicitMH :: MonadSample m => Int -> a -> (a -> m a) -> (a -> Double) -> m [a]
explicitMH numSteps initial proposal likelihood = P.toListM $ stochastically numSteps
                initial
                (mhStep
                  proposal
                  likelihood )

-- | This is the more general version of MH MCMC
-- | that makes use of monad-bayes's Traced inference transformer.
traceMH :: MonadSample m => Int -> Static.Traced m a -> m [a]
traceMH = Static.mh