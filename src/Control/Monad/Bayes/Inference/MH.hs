{-# LANGUAGE LambdaCase #-}
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

import Control.Monad.Bayes.Class (MonadSample (bernoulli, normal))
import qualified Pipes.Prelude as P
import Pipes.Core (Producer)
import Pipes ((>->), runEffect)
import qualified Control.Monad.Bayes.Traced.Static as Static
import Control.Monad.Bayes.Sampler (sampleIO, SamplerIO)
import Data.Either (isRight, isLeft)
import qualified Pipes as P
import Brick
import Brick.Widgets.ProgressBar (progressBar)

-- | Trace MH (as defined in Control.Monad.Bayes.Traced) works for any probabilistic program, 
-- | but this comes at the cost of expressive power, where proposals are concerned.
-- | If instead you can specify an explicit proposal and scoring function, you can use explicitMH. 
-- | This explicitly runs a Markov Chain andis implemented using Pipes, which is a library
-- | for handling effectful lists lazily.

-- The Metropolis-Hastings criterion 
-- (see https://ermongroup.github.io/cs228-notes/inference/sampling/)
mhStep :: MonadSample m => (a -> m a) -> (a -> Double) -> (a -> m (Either a a))
mhStep proposal likelihood currentState = do
  proposed <- proposal currentState
  let score = likelihood proposed
      oldScore = likelihood currentState
      ratio = min 1 (score / oldScore)
  accept <- bernoulli ratio
  return (if accept then Right proposed else Left currentState)

walk :: Monad m => Int -> a -> (a -> m a) -> Producer a m ()
walk i initial transition =
    P.unfoldr (fmap (Right . (\a -> (a,a))) . transition) initial
    >-> P.take i

explicitMH :: MonadSample m => Int -> a -> (a -> m a) -> (a -> Double) -> Producer (Either a a) m ()
explicitMH numSteps initial proposal likelihood =
  let walk i initial transition = P.unfoldr (fmap (Right . (\a -> (a, either id id a))) . mhStep transition likelihood) initial
        >-> P.take i
  in walk numSteps initial proposal

data MCMCData a = MCMCData {numSteps :: Int, numSuccesses :: Int, samples :: [a]} deriving (Show)

runExplicitMH numSteps initial proposal likelihood = runEffect $ 
  -- samples <- P.fold (\ls x -> either id id x : ls) [] id $
    (P.hoist sampleIO (explicitMH numSteps initial proposal likelihood) 
    >-> P.scan (\(MCMCData ns nsc smples) a -> MCMCData {numSteps = ns + 1, numSuccesses = nsc + (if isRight a then 1 else 0), samples = either id id a : smples }) 
      (MCMCData {numSteps = 0, numSuccesses = 0, samples = []}) id) >-> P.take numSteps
    >-> P.mapM_ print 
  -- print (length samples)
  -- return samples
te = runExplicitMH 10 4 (`normal` 1) (\x -> x * 0.00000001)

try :: Producer a IO b
try = P.hoist sampleIO undefined

-- todo: make an effort to initialize by sampling, and if it fails, report informative error
-- stop the mcmc once there's convergence: concurrency, so that you can hit a button and stop
-- the chain when it looks good
-- GOOD: use brick to display nice command line stuff

-- | This is the more general version of MH MCMC
-- | that makes use of monad-bayes's Traced inference transformer.
traceMH :: MonadSample m => Int -> Static.Traced m a -> m [a]
traceMH = Static.mh


ui :: Widget ()
ui = progressBar (Just "foo") 20

br :: IO ()
br = simpleMain ui