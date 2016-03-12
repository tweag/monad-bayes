module TestInference where

import Data.AEq
import Control.Monad.Trans.Identity
import System.Random

import Base
import Dist
import Sampler
import Empirical
import Inference
import Sprinkler

sprinkler :: MonadBayes m => m Bool
sprinkler = Sprinkler.soft

g = mkStdGen 0

check_terminate_smc = stdSample (smc' 2 5 sprinkler) g

check_preserve_smc = (enumerate . runIdentityT . transform . smc 2 2) sprinkler ~==
                      enumerate sprinkler
