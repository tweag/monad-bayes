module TestSequential where

import Data.AEq

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator as Dist
import Control.Monad.Bayes.Sequential
import Sprinkler

twoSync :: MonadInfer m => m Int
twoSync = do
  x <- uniformD[0,1]
  factor (fromIntegral x)
  y <- uniformD[0,1]
  factor (fromIntegral y)
  return (x+y)

finishedTwoSync :: MonadInfer m => Int -> m Bool
finishedTwoSync n = finished (run n twoSync) where
  run 0 d = d
  run k d = run (k-1) (advance d)

checkTwoSync :: Int -> Bool
checkTwoSync 0 = mass (finishedTwoSync 0) False ~== 1
checkTwoSync 1 = mass (finishedTwoSync 1) False ~== 1
checkTwoSync 2 = mass (finishedTwoSync 2) True  ~== 1
checkTwoSync _ = error "Unexpected argument"

sprinkler :: MonadInfer m => m Bool
sprinkler = Sprinkler.soft

checkPreserve :: Bool
checkPreserve = enumerate (finish sprinkler) ~== enumerate sprinkler

pFinished :: Int -> Double
pFinished 0 = 0.8267716535433071
pFinished 1 = 0.9988062077198566
pFinished 2 = 1
pFinished _ = error "Unexpected argument"

isFinished :: MonadInfer m => Int -> m Bool
isFinished n = finished (run n sprinkler) where
  run 0 d = d
  run k d = run (k-1) (advance d)

checkSync :: Int -> Bool
checkSync n = mass (isFinished n) True ~== pFinished n
