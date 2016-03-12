module TestParticle where

import Data.AEq

import Base
import Dist
import Particle
import Sprinkler

two_sync :: MonadBayes m => m Int
two_sync = do
  x <- uniformD[0,1]
  factor (fromIntegral x)
  y <- uniformD[0,1]
  factor (fromIntegral y)
  return (x+y)

finished_two_sync n = finished (run n two_sync) where
  run 0 d = d
  run n d = run (n-1) (advance d)
check_two_sync 0 = mass (finished_two_sync 0) False ~== 1
check_two_sync 1 = mass (finished_two_sync 1) False ~== 1
check_two_sync 2 = mass (finished_two_sync 2) True  ~== 1

sprinkler :: MonadBayes m => m Bool
sprinkler = Sprinkler.soft

check_preserve = enumerate (flatten sprinkler) ~== enumerate sprinkler

p_finished 0 = 0.8267716535433071
p_finished 1 = 0.9988062077198566
p_finished 2 = 1
is_finished n = finished (run n sprinkler) where
  run 0 d = d
  run n d = run (n-1) (advance d)
check_sync n = mass (is_finished n) True ~== p_finished n
