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

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Inference
import Control.Monad.Bayes.Population

import System.IO
import Control.Arrow (second)

ns = [1..10]

-- \ KL divergence between two distributions.
-- The distributions should be normalized to ensure correct results.
kl :: (Real r, NumSpec r, Ord a) => Dist r a -> Dist r a -> r
kl p q = expectation f p where
  f x = log $ dp x / dq x
  dp = mass p
  dq = mass q

main = do
  -- make sure `putStrLn` prints to console immediately
  hSetBuffering stdout LineBuffering

  -- pimhSamples <- pimh 10 100 1000 DPmixture.dpMemClusters
  -- let pimhResults = map ((`kl` DPmixture.posteriorClustersDist) . uniformD . (`take` pimhSamples) . (*100)) ns
  -- putStrLn $ show $ pimhResults

  smcSamples <- sampleIO $ sequence $ replicate 10 $ fmap (map (second fromLogDomain)) $ runPopulation $ smc 16 100 $ fmap head HMM.hmm
  let smcResults = map ((`kl` (head (tail HMM.exactMarginals))) . categorical . concat . (`take` smcSamples)) ns
  putStrLn $ show $ smcResults

  --putStrLn $ show $ Dist.explicit $ DPmixture.posteriorClustersDist

  --(xs,w) <- runWeighted HMM.hmm
  --putStrLn $ show $ xs

  --putStrLn $ show $ map Dist.explicit $ HMM.exactMarginals
