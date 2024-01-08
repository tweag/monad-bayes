module Control.Monad.Bayes.Inference.Lazy.WIS where

import Control.Monad.Bayes.Sampler.Lazy (SamplerT, weightedSamples)
import Control.Monad.Bayes.Weighted (WeightedT)
import Numeric.Log (Log (Exp))
import System.Random (Random (randoms), getStdGen, newStdGen)

-- | Weighted Importance Sampling

-- | Likelihood weighted importance sampling first draws n weighted samples,
--    and then samples a stream of results from that regarded as an empirical distribution
lwis :: Int -> WeightedT (SamplerT IO) a -> IO [a]
lwis n m = do
  xws <- weightedSamples m
  let xws' = take n $ accumulate xws 0
  let max' = snd $ last xws'
  _ <- newStdGen
  rs <- randoms <$> getStdGen
  return $ fmap (\r -> fst $ head $ filter ((>= Exp (log r) * max') . snd) xws') rs
  where
    accumulate :: (Num t) => [(a, t)] -> t -> [(a, t)]
    accumulate ((x, w) : xws) a = (x, w + a) : (x, w + a) : accumulate xws (w + a)
    accumulate [] _ = []
