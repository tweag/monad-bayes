{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}



{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}



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


import Control.Monad (void)
-- #if !(MIN_VERSION_base(4,11,0))
-- import Data.Monoid
-- #endif
import qualified Graphics.Vty as V

import qualified Brick.AttrMap as A
import qualified Brick.Main as B
import qualified Brick.Types as T
import qualified Brick.Widgets.ProgressBar as P


import Control.Monad.Bayes.Class (MonadSample (bernoulli, normal, random, poisson, beta, gamma), normalPdf, MonadInfer, condition, factor)
import qualified Pipes.Prelude as P
import Pipes.Core (Producer)
import Pipes ((>->), runEffect, MonadTrans (lift), (<-<))
import qualified Control.Monad.Bayes.Traced.Static as Static
import Control.Monad.Bayes.Sampler (sampleIO, SamplerIO)
import qualified Pipes as P
import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO)
import Brick.Widgets.Border.Style
import Brick.Widgets.Border
import Brick.Widgets.Center
import Numeric.Log hiding (sum)
import Graphics.Vty (yellow, charFill, pad, crop, cropTop, horizJoin, horizCat)
import GHC.Float (double2Float)
import Control.Monad.Bayes.Traced.Common (mhTrans, Trace (Trace, variables, density), mhTransWithBool)
import Control.Monad.Bayes.Traced.Static
import Control.Monad.Bayes.Weighted (runWeighted, prior, Weighted)
import Control.Monad.Bayes.Free (withPartialRandomness)
import Control.Monad.Bayes.Enumerator (toBin)
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace (trace)
import Data.List (sort)
import Data.Void (Void)
import Control.Foldl (mean, fold, Fold (Fold))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Foldl as Fold




-- | Trace MH (as defined in Control.Monad.Bayes.Traced) works for any probabilistic program, 
-- but this comes at the cost of expressive power, where proposals are concerned.
-- If instead you can specify a proposal and scoring function manually, you can use manualMH. 
-- This explicitly runs a Markov Chain on the return value of your probabilistic program


data MCMCData a = MCMCData {
  numSteps :: Int,
  numSuccesses :: Int,
  samples :: [a],
  lk :: [Double],
  totalSteps :: Int
  } deriving (Show)

-- The Metropolis-Hastings criterion 
-- (see https://ermongroup.github.io/cs228-notes/inference/sampling/)
manualMhStep :: MonadSample m => (a -> m a) -> (a -> Double) -> (a -> m (MHResult a))
manualMhStep proposal likelihood currentState = do
  proposed <- proposal currentState
  let score = likelihood proposed
      oldScore = likelihood currentState
      ratio = min 1 (score / oldScore)
  accept <- bernoulli $ clamp 0 1 ratio
  return (if accept then MHResult proposed True score else MHResult currentState False oldScore)

data MHResult a = MHResult {value :: a, success :: Bool, likelihood :: Double}

manualMH :: MonadSample m => Int -> a -> (a -> m a) -> (a -> Double) -> Producer (MHResult a) m ()
manualMH i initial proposal likelihood =
  P.unfoldr (fmap (Right . (\a -> (a, value a))) . manualMhStep proposal likelihood) initial
   >-> P.take i



-- todo: make an effort to initialize by sampling, and if it fails, report informative error
-- stop the mcmc once there's convergence: concurrency, so that you can hit a button and stop
-- the chain when it looks good

-- | This is the more general version of MH MCMC
-- | that makes use of monad-bayes's Traced inference transformer.
-- traceMH :: MonadSample m => Int -> Static.Traced m a -> m [a]
-- traceMH = Static.mh


independentSamples :: Monad m => m (Traced m a1) -> P.Proxy a' a2 () [Double] m ()
independentSamples m =
  P.repeatM m 
  >-> P.mapM traceDist
  >-> P.dropWhile ((== 0) . density)
  >-> P.map variables
  >-> P.take 10



-- | convert a probabilistic program into a producer of samples
runTraced :: MonadSample m => Traced m a -> P.Producer (MHResult a) m ()
runTraced (Traced w d) = 
  P.map (\(Trace _ output density,b) -> MHResult output b (exp $ ln density)) 
  <-< do
    tr <- lift d
    P.unfoldr (fmap (Right . (\ k -> (k, fst k))) . mhTransWithBool w) tr

-- | Brick is a terminal user interface (TUI)
-- which we use to display inference algorithms in progress

-- | draw the brick app
drawUI :: MCMCData Double -> [Widget ()]
drawUI state = [ui] where

  completionBar = updateAttrMap
          (A.mapAttrNames [ (doneAttr, P.progressCompleteAttr)
                          , (toDoAttr, P.progressIncompleteAttr)
                          ]
          ) $ toBar $ fromIntegral $ numSteps state

  likelihoodBar = updateAttrMap
          (A.mapAttrNames [ (doneAttr, P.progressCompleteAttr)
                          , (toDoAttr, P.progressIncompleteAttr)
                          ]
          ) $ P.progressBar (Just $ "Mean likelihood for last 1000 samples: " <> take 10 (show (head $ lk state <> [0])))
              (double2Float (fold mean $ take 1000 $ lk state) / double2Float (maximum $ 0 : lk state))

  displayStep c = Just $ "Step " <> show c
  numFailures = numSteps state - numSuccesses state
  toBar v = P.progressBar (displayStep v) (v / fromIntegral (totalSteps state  ))
  displaySuccessesAndFailures = withBorderStyle unicode $
        borderWithLabel (str "Successes and failures") $
        center (str (show $ numSuccesses state))
        <+>
        vBorder
        <+>
        center (str (show numFailures))
  warning = if numSteps state > 1000 && (fromIntegral (numSuccesses state) / fromIntegral (numSteps state)) < 0.1
      then  withAttr (attrName "highlight") $ str "Warning: acceptance rate is rather low. This probably means that your proposal isn't good."
      else str ""

  dict = fold (foldByKeyMap Fold.sum) ((,1) . toBin 0.05 <$> take 10000 (samples state) )
  valSum = fromIntegral $ sum $ M.elems dict
  bins = M.keys dict
  ndict = M.map ((/valSum) . fromIntegral) dict
  ui =

      (str "Progress: " <+> completionBar)
      <=>
      (str "Likelihood: " <+> likelihoodBar)
      <=>
      str "\n"
      <=>
      displaySuccessesAndFailures
      <=>
      warning
      <=>
      withBorderStyle unicode (
          raw $ 
            horizCat [makeBar bin ndict | bin <- sort bins ])
        
-- TODO: once foldl is updated to latest version, this function will be in its library
foldByKeyMap :: forall k a b. Ord k => Fold a b -> Fold (k, a) (Map k b)
foldByKeyMap f = case f of
  Fold (step0 :: x -> a -> x) (ini0 :: x) (end0 :: x -> b) ->
    let
      step :: Map k x -> (k,a) -> Map k x
      step mp (k,a) = Map.alter addToMap k mp where
        addToMap Nothing         = Just $ step0 ini0 a
        addToMap (Just existing) = Just $ step0 existing a

      ini :: Map k x
      ini = Map.empty

      end :: Map k x -> Map k b
      end = fmap end0
    in Fold step ini end

makeBar :: (RealFrac a, Ord k) => k -> M.Map k a -> V.Image
makeBar bin dict = 
            cropTop 10 $
            pad 0 10 0 0 $
            charFill (fg yellow) '.' 1 ( 1 * maybe 0 (round . (* 100)) (M.lookup bin dict))

-- mapInto :: Foldable t => t (Double, Double) -> M.Map (Double, Double) Int
-- mapInto = foldr (M.alter (\case Nothing -> Just 0; Just i -> Just (i + 1) )) (M.empty :: M.Map (Double, Double) Int )


-- | handler for events received by the TUI
appEvent :: MCMCData Double -> T.BrickEvent () (MCMCData Double) -> T.EventM () (T.Next (MCMCData Double))
appEvent p (T.VtyEvent e) =
    case e of
         V.EvKey (V.KChar 'q') [] -> B.halt p
         _ -> B.continue p
appEvent _ (AppEvent d) = B.continue d
appEvent _ _ = error "unknown event"

doneAttr, toDoAttr :: A.AttrName
doneAttr = A.attrName "theBase" <> A.attrName "done"
toDoAttr = A.attrName "theBase" <> A.attrName "remaining"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
         [ (A.attrName "theBase",                bg V.brightBlack)
         , (doneAttr,                   V.black `on` V.white)
         , (toDoAttr,                   V.white `on` V.black)
         , (attrName "highlight",       fg yellow)
         ]

mcmcTUI :: B.App (MCMCData Double) (MCMCData Double) ()
mcmcTUI =
    B.App { B.appDraw = drawUI
          , B.appChooseCursor = B.showFirstCursor
          , B.appHandleEvent = appEvent
          , B.appStartEvent = return
          , B.appAttrMap = const theMap
          }


tui :: IO ()
tui = void do

      -- model = manualMH n 0.0 (`normal` 1) (exp . ln . normalPdf 20 1)
  eventChan <- newBChan 10
  initialVty <- buildVty
  _ <- forkIO $ run (runTraced exact) eventChan n
  B.customMain initialVty buildVty (Just eventChan) mcmcTUI (initialState n)

  where
    
  buildVty = V.mkVty V.defaultConfig
  n = 100000
  initialState n = MCMCData {numSteps = 0, samples = [], lk = [], numSuccesses = 0, totalSteps = n}
  
  run prod chan i = runEffect $
      P.hoist (sampleIO . prior) prod
      >-> P.scan
        (\mcmcdata@(MCMCData ns nsc smples lk _) a ->
          mcmcdata {
            numSteps = ns + 1,
            numSuccesses = nsc + if success a then 1 else 0,
            samples = value a : smples, lk = likelihood a : lk
              })
        (initialState i) 
        id 
      >-> P.take i
      >-> P.mapM_ (writeBChan chan)



distribution :: Traced (Weighted SamplerIO) Double
distribution = do
  y <- gamma 1 1 
  condition (y < 2) 
  return y


points :: [Double]
points = [0.8, 0.2, -0.6, 0.45, -0.3]


-- model :: (MonadBayes m, CustomReal m ~ Double) => m Double
approx :: MonadInfer m => m Double
approx = do
  prec <- gamma 1 1
  let stddev = sqrt (1 / prec)
  let noise = normalPdf 0 stddev
  mapM_ (factor . noise) points
  return prec

-- | Exact posterior for the model.
-- For derivation see Kevin Murphy's
-- "Conjugate Bayesian analysis of the Gaussian distribution"
-- section 4.
-- exact :: (MonadDist m, CustomReal m ~ Double) => m Double
exact :: MonadInfer m => m Double
exact = gamma a b
  where
    a = 1 + fromIntegral (length points) / 2
    b = 1 + sum (map (^ (2 :: Int)) points) / 2


-- example :: MonadInfer m => m Double
-- example = do
--   y <- normal 0 1
--   factor $ log $ Exp y
--   return y

-- running = sampleIO $ prior $ Static.mh 10 example 
