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


-- paramPrior = do 
--     slope <- N.traced "slope" $ normal 0 2
--     intercept <- N.traced "intercept" $ normal 0 2
--     noise <- N.traced "noise" $ gamma 1 1
--     prob_outlier <- uniform 0 0.5 
--     return (slope, intercept, noise, prob_outlier)

forward :: (MonadState [T.Text] m, MonadTrans t,
  MonadSample (t m), Num a, Floating c) =>
  (a, a, c, Double) -> a -> t m ((a, c), Bool)
forward (slope, intercept, noise, probOutlier) x = do
    isOutlier <- bernoulli probOutlier
    let meanParams = if isOutlier
                    then (0, 20)
                    else (x*slope + intercept, sqrt noise)
    return (meanParams, isOutlier)

range = [-10,-9.9..10] :: [Double]


-- xs :: [Double]
-- ys :: [Double]


-- regressionWithOutliersData :: (MonadSample m, Traversable t) => t Double -> m (t ((Double, Double), Bool))
regressionWithOutliersData :: (Traversable t1, MonadState [T.Text] m, MonadTrans t2,
  MonadSample (t2 m)) =>
  t1 Double -> t2 m (t1 ((Double, Double), Bool))
regressionWithOutliersData xs = do
    params <- do
      slope <- normal 0 2
      intercept <- normal 0 2
      noise <- gamma 1 1
      prob_outlier <- uniform 0 0.5 
      return (slope, intercept, noise, prob_outlier)

    forM xs \x -> do
        ((mu, std), isOutlier) <- forward params x
        y <- normal mu std
        return ((x, y), isOutlier)

lower m = prior $ Control.Monad.Bayes.Weighted.hoist (flip evalStateT []) m

(xys, bs) = unzip $ sampleSTfixed $ lower $ regressionWithOutliersData range



-- regressionWithOutliers :: (MonadSample m, MonadCond m) =>
--     [Double] -> [Double] -> m ((Double, Double, Double, Double), [Bool])
regressionWithOutliers :: (MonadState [T.Text] m, MonadTrans t, MonadSample (t m),
  MonadCond (t m)) =>
  [Double] -> [Double] -> t m ((Double, Double, Double, Double), [Bool])
regressionWithOutliers xs ys = do
    params <- do 
      slope <- N.traced "slope" $ normal 0 2
      intercept <- N.traced "intercept" $ normal 0 2
      noise <- N.traced "noise" $ gamma 1 1
      prob_outlier <- uniform 0 0.5 
      return (slope, intercept, noise, prob_outlier)
    
    outliers <- forM (zip3 xs ys [0..]) \(x, y, i) -> do
        ((mu, std), isOutlier) <- N.traced (T.pack $ show i) $ forward params x
        factor $ normalPdf mu std y
        return isOutlier
    return (params, outliers)

simpleProposal oldProposal = do
  updateSlope <- bernoulli 0.1
  if updateSlope then do 
      traceM "update slope"
      newSlope <- random
      newIntercept <- random
      newNoise <- random
      return $ M.union (M.fromList [
        (["slope"], newSlope), 
        (["intercept"], newIntercept), 
        (["noise"], newNoise)]) oldProposal
  else do 
    i <- uniformD [0..length (M.keys oldProposal)]
    traceM $ "update " <> show i
    val <- random
    -- vals <- replicateM (length (M.keys oldProposal) - 3) random
    return $ M.union (M.fromList $ zip [[T.pack $ show i, "bern"]] [val]) oldProposal



-- prop c = do
--   -- let oldSlope = fromMaybe undefined $ M.lookup ["slope"] c
--   -- newSlope <- max 0.0000000001 . min 1 <$> normal oldSlope 0.01
--   -- let oldIntercept = fromMaybe undefined $ M.lookup ["intercept"] c
--   -- newIntercept <- max 0.0000000001 . min 1 <$> normal oldIntercept 0.01
--   -- slope <- random
--   -- intercept <- random
--   -- return $ M.fromList [(["slope"], newSlope), (["intercept"], newIntercept)]
--   key <- uniformD $ M.keys c
--   -- traceM $ show ("foo", key)
--   let old = fromMaybe undefined $ M.lookup key c
--   new <- max 0.0000000001 . min 1 <$> normal old 0.05
--   -- s <- random
--   return $ M.insert key new c


ex :: IO ()
ex = do 
  x <- sampleIO $ prior $ P.toListM $ runTracedNamed' (simpleProposal) 0 (uncurry regressionWithOutliers $ unzip xys)
  TL.writeFile "data/tui_output.txt" (pShowNoColor x)

-- ex = do 
--   x <- sampleIO $ prior $ mhWithBool 5000 $ (regression xs ys)
--   TL.writeFile "data/tui_output.txt" (pShowNoColor x) 
