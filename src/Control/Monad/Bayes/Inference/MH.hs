{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}



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


import Control.Monad.Bayes.Class (MonadSample (bernoulli))
import Pipes.Prelude qualified as P
import Pipes qualified as P
import Pipes.Core (Producer)
import Pipes ((>->))

import Brick ( clamp )
import Numeric.Log ( Log(Exp) )

import Control.Monad.Bayes.Traced.Common
    ( Trace(Trace, output, density),
      mhTransWithBool,
      MHResult(MHResult, trace) ) 
import Control.Monad.Bayes.Traced.Static ( Traced(Traced) )



-- | Trace MH (as defined in Control.Monad.Bayes.Traced) works for any probabilistic program, 
-- but this comes at the cost of expressive power, where proposals are concerned.
-- If instead you can specify a proposal and scoring function manually, you can use manualMH. 
-- This explicitly runs a Markov Chain on the return value of your probabilistic program

manualMhStep :: MonadSample m => (a -> m a) -> (a -> Double) -> (a -> m (MHResult a))
manualMhStep proposal likelihood currentState = do
  proposed <- proposal currentState
  let score = likelihood proposed
      oldScore = likelihood currentState
      ratio = min 1 (score / oldScore)
  accept <- bernoulli $ clamp 0 1 ratio
  return 
    if accept 
      then MHResult True (Trace [] proposed (log $ Exp score)) 
      else MHResult False (Trace [] currentState (log $ Exp oldScore))

manualMH :: MonadSample m => Int -> a -> (a -> m a) -> (a -> Double) -> Producer (MHResult a) m ()
manualMH i initial proposal likelihood =
  P.unfoldr (fmap (Right . (\a -> (a, output $ trace a))) . manualMhStep proposal likelihood) initial
   >-> P.take i

-- todo: do we need to initialize the MCMC? is that important?

-- | draw iid samples until you get one that has non-zero likelihood
independentSamples :: Monad m => Traced m a -> Producer (MHResult a) m (Trace a)
independentSamples (Traced w d) = 
  P.repeatM d 
  >-> P.takeWhile' ((== 0) . density)
  >-> P.map (MHResult False)

-- | convert a probabilistic program into a producer of samples
runTraced :: MonadSample m => Int -> Traced m a -> P.Producer (MHResult a) m ()
runTraced burnIn m@(Traced w d) = do
  initialValue <- independentSamples m >-> P.drain
  (P.unfoldr (fmap (Right . (\ k -> (k , trace k))) . mhTransWithBool w) initialValue
    >-> P.drop burnIn)