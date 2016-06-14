module TestEmpirical where

import System.Random
import Data.AEq
import Control.Monad.Trans.Identity

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Dist
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Empirical as Empirical
import Sprinkler

g = mkStdGen 0

pop_size = flip stdSample g $ weightedSampleSize $ spawn 5 >> sprinkler

many_size = flip stdSample g $ weightedSampleSize $ spawn 5 >> sprinkler >> spawn 3

sprinkler :: MonadBayes m => m Bool
sprinkler = Sprinkler.soft
sprinkler_exact = enumerate Sprinkler.soft

--all_check = (mass (Empirical.all id (spawn 2 >> sprinkler)) True) ~== 0.09

trans_check1 = enumerate (runIdentityT (collapse sprinkler)) ~==
               sprinkler_exact
trans_check2 = enumerate (runIdentityT (collapse (spawn 2 >> sprinkler))) ~==
               sprinkler_exact

resample_check n =
  (enumerate . runIdentityT . collapse . resampleN n) (spawn 2 >> sprinkler) ~==
  sprinkler_exact
