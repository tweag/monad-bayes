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

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler.Strict
import qualified Control.Monad.Bayes.TracedT.Basic as Basic
import Control.Monad.Bayes.TracedT.Common (MHResult (..), Trace (..), burnIn, mhTransWithBool)
import qualified Control.Monad.Bayes.TracedT.Dynamic as Dynamic
import qualified Control.Monad.Bayes.TracedT.Static as Static
import Control.Monad.Bayes.Weighted
import Control.Monad.Trans (lift)
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P

data Proposal = SingleSiteMH

data MCMCConfig = MCMCConfig {proposal :: Proposal, numMCMCSteps :: Int, numBurnIn :: Int}

defaultMCMCConfig :: MCMCConfig
defaultMCMCConfig = MCMCConfig {proposal = SingleSiteMH, numMCMCSteps = 1, numBurnIn = 0}

mcmc :: MonadSample m => MCMCConfig -> Static.TracedT (Weighted m) a -> m [a]
mcmc (MCMCConfig {..}) m = burnIn numBurnIn $ unweighted $ Static.mh numMCMCSteps m

mcmcBasic :: MonadSample m => MCMCConfig -> Basic.TracedT (Weighted m) a -> m [a]
mcmcBasic (MCMCConfig {..}) m = burnIn numBurnIn $ unweighted $ Basic.mh numMCMCSteps m

mcmcDynamic :: MonadSample m => MCMCConfig -> Dynamic.TracedT (Weighted m) a -> m [a]
mcmcDynamic (MCMCConfig {..}) m = burnIn numBurnIn $ unweighted $ Dynamic.mh numMCMCSteps m

-- -- | draw iid samples until you get one that has non-zero likelihood
independentSamples :: Monad m => Static.TracedT m a -> P.Producer (MHResult a) m (Trace a)
independentSamples (Static.TracedT w d) =
  P.repeatM d
    >-> P.takeWhile' ((== 0) . probDensity)
    >-> P.map (MHResult False)

-- | convert a probabilistic program into a producer of samples
mcmcP :: MonadSample m => MCMCConfig -> Static.TracedT m a -> P.Producer (MHResult a) m ()
mcmcP MCMCConfig {..} m@(Static.TracedT w _) = do
  initialValue <- independentSamples m >-> P.drain
  ( P.unfoldr (fmap (Right . (\k -> (k, trace k))) . mhTransWithBool w) initialValue
      >-> P.drop numBurnIn
    )
