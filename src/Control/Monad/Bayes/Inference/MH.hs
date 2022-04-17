{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}



{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
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

-- | Trace MH (as defined in Control.Monad.Bayes.Traced) works for any probabilistic program, 
-- but this comes at the cost of expressive power, where proposals are concerned.
-- If instead you can specify a proposal and scoring function manually, you can use manualMH. 
-- This explicitly runs a Markov Chain on the return value of your probabilistic program


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

data MCMCData a = MCMCData {
  numSteps :: Int,
  numSuccesses :: Int,
  samples :: [a],
  lk :: [Double],
  totalSteps :: Int
  } deriving (Show)

-- todo: make an effort to initialize by sampling, and if it fails, report informative error
-- stop the mcmc once there's convergence: concurrency, so that you can hit a button and stop
-- the chain when it looks good

-- | This is the more general version of MH MCMC
-- | that makes use of monad-bayes's Traced inference transformer.
-- traceMH :: MonadSample m => Int -> Static.Traced m a -> m [a]
-- traceMH = Static.mh

-- independentSamples :: MonadInfer m => Traced m a -> P.Producer a (Traced m) ()
-- independentSamples :: MonadSample m =>
--   (MonadInfer m => m a)
--   -> P.Producer
--       ((a, Log Double), [Double])
--       m
--       (( a, Log Double), [Double])
-- independentSamples :: MonadInfer m =>
--   m (Traced m a)
--   -> P.Producer ((a, Log Double), [Double]) m ()
search m = 
  P.unfoldr step m where
    step x = do
      x' <- x
      al <- traceDist x'
      return if density al > 0 
        then Left al
        else Right (al, m)

foo :: MonadSample m => Traced m a -> P.Producer (MHResult a) m r
foo m@(Traced w d) = 
  P.map (\(Trace variables output density,b) -> MHResult output b (exp $ ln density)) <-< do
  -- tr <- search (return m) >-> P.map (, True) 
  tr <- lift d
  let step x = Right . (\k -> (  k, (fst k))) <$> mhTransWithBool w x
  P.unfoldr step tr

independentSamples :: Monad m => m (Traced m a1) -> P.Proxy a' a2 () [Double] m ()
independentSamples m =
  P.repeatM m 
  >-> P.mapM traceDist  -- (withPartialRandomness [] . runWeighted . model)
  >-> P.dropWhile ((== 0) . density)
  >-> P.map variables
  >-> P.take 10

a :: P.MonadIO m => m (Traced m a) -> m ()
a m = P.runEffect $ independentSamples m >-> P.print


b = sampleIO $ runWeighted $ a $ return do
  x <- bernoulli 0.1
  condition x
  return x

-- traceMH :: MonadSample m => Int -> a -> (a -> m a) -> (a -> Double) -> Producer (MHResult a) m ()
-- traceMH i p =
--   P.unfoldr (fmap (Right . (\a -> (a, a))) . mhTrans p) initial
--    >-> P.take i



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
              (double2Float (mean $ take 1000 $ lk state) / double2Float (maximum $ 0 : lk state))

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

  dict = mapInto (toBin 0.05 <$> take 10000 (samples state) )
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
        

makeBar bin dict = cropTop 10 $
            pad 0 10 0 0 $
            charFill (fg yellow) '.' 1 ( (* 1) $ fromMaybe 0 ((round . (*100)) <$> M.lookup bin dict))

mapInto :: Foldable t => t (Double, Double) -> M.Map (Double, Double) Int
mapInto = foldr (M.alter (\case Nothing -> Just 0; Just i -> Just (i + 1) )) (M.empty :: M.Map (Double, Double) Int )

mean :: (Fractional a, Foldable t) => t a -> a
mean ls = Prelude.sum ls / fromIntegral (length ls)



appEvent :: MCMCData Double -> T.BrickEvent () (MCMCData Double) -> T.EventM () (T.Next (MCMCData Double))
appEvent p (T.VtyEvent e) =
    case e of
         V.EvKey (V.KChar 'q') [] -> B.halt p
         _ -> B.continue p
appEvent _ (AppEvent d) = B.continue d
appEvent _ _ = undefined



doneAttr, toDoAttr, theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"
doneAttr = theBaseAttr <> A.attrName "done"
toDoAttr = theBaseAttr <> A.attrName "remaining"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
         [ (theBaseAttr,                bg V.brightBlack)
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

-- example :: MonadInfer m => m Double
-- example = do
--   y <- normal 0 1
--   factor $ log $ Exp y
--   return y

-- running = sampleIO $ prior $ Static.mh 10 example 



tui :: IO ()
tui = void do
  let buildVty = V.mkVty V.defaultConfig
      n = 100000
      -- model = manualMH n 0.0 (`normal` 1) (exp . ln . normalPdf 20 1)
  eventChan <- newBChan 10
  initialVty <- buildVty
  _ <- forkIO $ run (foo $ distribution) eventChan n
  B.customMain initialVty buildVty (Just eventChan) mcmcTUI (initialState n)

  where
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

initialState :: Int -> MCMCData Double
initialState n = MCMCData {numSteps = 0, samples = [], lk = [], numSuccesses = 0, totalSteps = n}




-- distribution :: Traced SamplerIO Double
distribution :: Traced (Weighted SamplerIO) Double
distribution = do
  y <- gamma 1 1 
  condition (y < 2) 
  return y