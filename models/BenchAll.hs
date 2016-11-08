{-# LANGUAGE Rank2Types
 #-}

-- Import all models under maintenance.
-- Models not imported here will not be compiled
-- when invoking `stack bench`.
import qualified BetaBin
import qualified Dice
import qualified DPmixture
import qualified Gamma
import qualified HMM

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sampler
import qualified Control.Monad.Bayes.Dist as Dist
import Control.Monad.Bayes.Inference

import System.IO

main = do
  -- make sure `putStrLn` prints to console immediately
  hSetBuffering stdout LineBuffering

  --xs <- pimh 10 100 100 DPmixture.dpMemClusters
  --putStrLn $ show $ (Dist.enumerate :: Dist.Dist Double Int -> [(Int,Double)]) $ uniformD xs

  --putStrLn $ show $ Dist.explicit $ DPmixture.posteriorClustersDist

  (xs,w) <- runWeighted HMM.hmm
  putStrLn $ show $ xs

  putStrLn $ show $ map Dist.explicit $ HMM.exactMarginals
