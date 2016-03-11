module TestEmpirical where

import System.Random
import Data.AEq
import Control.Monad.Trans.Identity

import Base
import Dist
import Sampler
import Empirical
import Sprinkler

g = mkStdGen 0

pop_size = flip stdSample g $ population $ spawn 5 >> sprinkler

many_size = flip stdSample g $ population $ spawn 5 >> sprinkler >> spawn 3

sprinkler :: MonadBayes m => m Bool
sprinkler = Sprinkler.soft
sprinkler_exact = enumerate Sprinkler.soft

all_check = (mass (Empirical.all id (spawn 2 >> sprinkler)) True) ~== 0.09

trans_check1 = enumerate (runIdentityT (transform sprinkler)) ~==
               sprinkler_exact
trans_check2 = enumerate (runIdentityT (transform (spawn 2 >> sprinkler))) ~==
               sprinkler_exact

resample_check n =
  (enumerate . runIdentityT . transform . resampleN n) (spawn 2 >> sprinkler) ~==
  sprinkler_exact
