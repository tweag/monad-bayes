{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}



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


import Control.Monad.Bayes.Class (MonadSample (bernoulli, random, normal, gamma, uniform, uniformD), factor, normalPdf, MonadCond)
import Pipes.Prelude qualified as P
import Pipes qualified as P
import Pipes.Core (Producer)
import Pipes ((>->), MonadTrans (lift))

import Brick ( clamp )
import Numeric.Log ( Log(Exp) )

import Control.Monad.Bayes.Traced.Common
    ( Trace(Trace, output, density),
      mhTransWithBool,
      MHResult(MHResult, trace) ) 
import Control.Monad.Bayes.Traced.Static ( Traced(Traced), mh, hoistT, mhWithBool )
import Control.Monad.Bayes.Traced.Named qualified as N
import Control.Monad.State (evalStateT, MonadState)
import Control.Monad.State.Lazy (StateT)
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.Bayes.Sampler (sampleIO, sampleIOfixed, sampleSTfixed)
import qualified Data.Text.Lazy.IO as TL
import Text.Pretty.Simple (pShowNoColor)
import Control.Monad (forM, forM_)
import Control.Monad.Bayes.Weighted
import Debug.Trace (traceM)
import Data.Maybe
import qualified Control.Monad.Bayes.Traced.Static as T


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

-- | convert a probabilistic program into a producer of samples
-- runTracedNamed :: MonadSample m => Int -> N.Traced m a -> P.Producer (MHResult a) m ()
runTracedNamed :: MonadSample m =>
  N.Proposal m
  -> Int
  -> N.Traced
      (StateT [T.Text] m)
      a
  -> P.Producer (MHResult a) m b
runTracedNamed proposal burnIn m@(N.Traced w d) = do
  -- initialValue <- independentSamples m >-> P.drain
  ((P.unfoldr (fmap (Right . (\ k -> (k , fst k))) . N.mhTransWithBool proposal w) =<< (lift $ evalStateT d []))
    >-> P.map (\(N.ChoiceMap {..}, b) -> MHResult b (Trace (M.elems cm) output density))
    >-> P.drop burnIn)

-- | convert a probabilistic program into a producer of samples
-- runTracedNamed :: MonadSample m => Int -> N.Traced m a -> P.Producer (MHResult a) m ()
-- runTracedNamed' :: MonadSample m =>
--   N.Proposal m
--   -> Int
--   -> N.Traced
--       (StateT [T.Text] m)
--       a
--   -> P.Producer (MHResult a) m b
runTracedNamed' proposal burnIn m@(N.Traced w d) = do
  -- initialValue <- independentSamples m >-> P.drain
  (P.unfoldr (fmap (Right . (\ k -> (k , fst k))) . N.mhTransWithBool proposal w) =<< (lift $ evalStateT d []))
    >-> P.drop burnIn
    >-> P.take 500


