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


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
import Control.Monad (void)
-- #if !(MIN_VERSION_base(4,11,0))
-- import Data.Monoid
-- #endif
import qualified Graphics.Vty as V

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.ProgressBar as P


import Control.Monad.Bayes.Class (MonadSample (bernoulli, normal), normalPdf)
import qualified Pipes.Prelude as P
import Pipes.Core (Producer)
import Pipes ((>->), runEffect)
import qualified Control.Monad.Bayes.Traced.Static as Static
import Control.Monad.Bayes.Sampler (sampleIO, SamplerIO)
import qualified Pipes as P
import Brick
import Brick.Widgets.ProgressBar (progressBar)
import Brick.BChan (newBChan, BChan, writeBChan)
import Control.Concurrent (forkIO)
import Brick.Widgets.Border.Style
import Brick.Widgets.Border
import Brick.Widgets.Center
import Numeric.Log
import Graphics.Vty (yellow, backgroundFill, charFill)

-- | Trace MH (as defined in Control.Monad.Bayes.Traced) works for any probabilistic program, 
-- | but this comes at the cost of expressive power, where proposals are concerned.
-- | If instead you can specify an explicit proposal and scoring function, you can use explicitMH. 
-- | This explicitly runs a Markov Chain andis implemented using Pipes, which is a library
-- | for handling effectful lists lazily.

-- The Metropolis-Hastings criterion 
-- (see https://ermongroup.github.io/cs228-notes/inference/sampling/)
mhStep :: MonadSample m => (a -> m a) -> (a -> Double) -> (a -> m (a, Bool, Double))
mhStep proposal likelihood currentState = do
  proposed <- proposal currentState
  let score = likelihood proposed
      oldScore = likelihood currentState
      ratio = min 1 (score / oldScore)
  accept <- bernoulli $ clamp 0 1 ratio
  return (if accept then (proposed, True, score) else (currentState, False, oldScore))

walk :: Monad m => Int -> a -> (a -> m a) -> Producer a m ()
walk i initial transition =
    P.unfoldr (fmap (Right . (\a -> (a,a))) . transition) initial
    >-> P.take i

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a

explicitMH :: MonadSample m => Int -> a -> (a -> m a) -> (a -> Double) -> Producer (a, Bool, Double) m ()
explicitMH numSteps initial proposal likelihood =
  let walk i initial transition = P.unfoldr (fmap (Right . (\a -> (a, fst3 a))) . mhStep transition likelihood) initial
        >-> P.take i
  in walk numSteps initial proposal

data MCMCData a = MCMCData {numSteps :: Int, numSuccesses :: Int, samples :: [a], lk :: [Double]} deriving (Show)

runExplicitMH :: Show a =>
  Int
  -> a
  -> (a -> SamplerIO a)
  -> (a -> Double)
  -> IO ()
runExplicitMH numSteps initial proposal likelihood = runEffect $
  -- samples <- P.fold (\ls x -> fst x : ls) [] id $
    (P.hoist sampleIO (explicitMH numSteps initial proposal likelihood)
    >-> P.scan (\(MCMCData ns nsc smples lk) a -> MCMCData {numSteps = ns + 1, numSuccesses = nsc + (if snd3 a then 1 else 0), samples = fst3 a : smples, lk = thd3 a : lk })
      (MCMCData {numSteps = 0, numSuccesses = 0, samples = [], lk = []}) id) >-> P.take numSteps
    >-> P.mapM_ print

thd3 :: (a, Bool, Double) -> Double
thd3 (_,_,c) = c

snd3 :: (a, Bool, Double) -> Bool
snd3 (_,b,_) = b
  -- print (length samples)
  -- return samples
te = runExplicitMH 10 0 (`normal` 1) (\x -> x * 0.00000001)

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


data MyAppState n = MyAppState { x :: Float, y :: Int, l :: [Double], len :: Int }

drawUI :: MyAppState () -> [Widget ()]
drawUI p = [ui]
    where
      -- use mapAttrNames
      xBar = updateAttrMap
             (A.mapAttrNames [ (xDoneAttr, P.progressCompleteAttr)
                             , (xToDoAttr, P.progressIncompleteAttr)
                             ]
             ) $ bar $ x p
      -- or use individual mapAttrName calls
      lbl c = Just $ "Step " <> (show $ fromEnum $ c * (fromIntegral $ len p))
      bar v = P.progressBar (lbl v) v
      a = withBorderStyle unicode $
            borderWithLabel (str "Successes and failures") $
            (center (str (show $ y p)) <+> vBorder <+> center (str (show ((len p) - y p))))
      ui = 
        
           
          graph (mean $ take 100 $ l p) <+> ((str "X: " <+> xBar) <=>
           str "\n" <=> 
           a <=>
           (if y p > 1000 
             then  withAttr (attrName "highlight") $ str "Warning: acceptance rate is rather low. This probably means that your proposal isn't good." else str "") <=>
           str ("Model likelihood: " <> show (l p)))

mean ls = Prelude.sum ls / fromIntegral (length ls)

graph n = withBorderStyle unicode $
            borderWithLabel (str "Plot") $
            (raw (charFill (fg yellow) 'O' 2 $ round $ 30 * n))

type E = MCMCData Double

appEvent :: MyAppState () -> T.BrickEvent () E -> T.EventM () (T.Next (MyAppState ()))
appEvent p (T.VtyEvent e) =
    let valid = clamp (0.0 :: Float) 1.0
    in case e of
        --  V.EvKey (V.KChar 'x') [] -> do
        --    y <- liftIO $ print 4
        --    M.continue $ p { x = valid $ x p + 0.05 }
         V.EvKey (V.KChar 'q') [] -> M.halt p
         _ -> M.continue p
appEvent p (AppEvent d) = M.continue $ p { x = fromIntegral (numSteps d) / (fromIntegral $ len p), y = numSuccesses d, l = lk d }

initialState :: Int -> MyAppState ()
initialState l = MyAppState 0.25 0 [] l

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

xDoneAttr, xToDoAttr :: A.AttrName
xDoneAttr = theBaseAttr <> A.attrName "X:done"
xToDoAttr = theBaseAttr <> A.attrName "X:remaining"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
         [ (theBaseAttr,               bg V.brightBlack)
         , (xDoneAttr,                 V.black `on` V.white)
         , (xToDoAttr,                 V.white `on` V.black)
         , (attrName "highlight", fg yellow)

        --  , (P.progressIncompleteAttr,  fg V.yellow)
         ]

theApp :: M.App (MyAppState ()) E ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

prog :: IO ()
prog = void do
  eventChan <- newBChan 10
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  let numSteps = 100000
  forkIO $ chain eventChan numSteps
  M.customMain initialVty buildVty (Just eventChan) theApp (initialState numSteps)

chain :: BChan E -> Int -> IO ()
chain chan numSteps = runEffect $
  -- samples <- P.fold (\ls x -> fst3 x : ls) [] id $
    (P.hoist sampleIO (explicitMH numSteps 0.0 (`normal` 1) (exp . ln . normalPdf 100 1))
    >-> P.scan (\(MCMCData ns nsc smples lk) a -> MCMCData {numSteps = ns + 1, numSuccesses = nsc + (if snd3 a then 1 else 0), samples = fst3 a : smples, lk = thd3 a : lk })
      (MCMCData {numSteps = 0, numSuccesses = 0, samples = [], lk = []}) id) >-> P.take numSteps
    >-> P.mapM_ (writeBChan chan)
