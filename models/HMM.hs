{-# LANGUAGE
 FlexibleContexts,
 TypeFamilies
 #-}

module HMM (
  values,
  hmm,
  exactMarginals,
  hmmKL
  ) where

--Hidden Markov Models

import Numeric.LinearAlgebra.HMatrix -- for the exact posterior only
import Data.Bifunctor (first, second)

import Numeric.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple
import qualified Control.Monad.Bayes.Enumerator as Dist

-- | States of the HMM
states :: [Int]
states = [-1,0,1]

-- | Observed values
values :: [Double]
values = [0.9,0.8,0.7,0,-0.025,5,2,0.1,0,
          0.13,0.45,6,0.2,0.3,-1,-1]

-- | The transition model.
trans :: MonadDist m => Int -> m Int
trans (-1) = categorical $ zip states [0.1, 0.4, 0.5]
trans 0    = categorical $ zip states [0.2, 0.6, 0.2]
trans 1    = categorical $ zip states [0.15,0.7,0.15]

-- | The emission model.
emission :: Int -> Normal Double
emission x = (normalDist (fromIntegral x) 1)

-- | Initial state distribution
start :: MonadDist m => m [Int]
start = uniformD $ map (:[]) states

-- | Example HMM from http://dl.acm.org/citation.cfm?id=2804317
hmm :: (MonadBayes m, CustomReal m ~ Double) => m [Int]
hmm = fmap reverse states where
  states = foldl expand start values
  --expand :: MonadBayes m => m [Int] -> Double -> m [Int]
  expand d y = do
    rest <- d
    x    <- trans $ head rest
    observe (emission x) y
    return (x:rest)


------------------------------------------------------------
-- Exact marginal posterior with forward-backward

-- \ KL divergence between two distributions.
-- The distributions should be normalized to ensure correct results.
kl :: (Real r, NumSpec r, Ord a) => Dist.Dist r a -> Dist.Dist r a -> r
kl p q = Dist.expectation f p where
  f x = log $ dp x / dq x
  dp = Dist.mass p
  dq = Dist.mass q

hmmKL :: [([Int], Double)]-> Double
hmmKL samples = result where
  -- convert to a list of marginal samples for each time step
  marginals = map (\i -> map (first (!! i)) samples) [1 .. (length HMM.values)]
  exact = HMM.exactMarginals
  result = if (length marginals == length exact)
             then Prelude.sum $ zipWith kl (map categorical marginals) exact
             else error $ "hmmKL: length mismatch " ++ show (length marginals) ++ " and " ++ show (length exact)

exactMarginals :: (MonadDist m, CustomReal m ~ Double) => [m Int]
exactMarginals = map marginal [0 .. length values - 1] where
    marginal i = categorical $ zip states $ map (lookup i) [0.. length states - 1]
    lookup = hmmExactMarginal initial tmatrix scores

initial :: Vector Double
initial = fromList $ map snd $ Dist.explicit $ start

tmatrix :: Matrix Double
tmatrix = fromColumns $ map (fromList . map snd . Dist.explicit . trans) states

scores :: [Vector Double]
scores = map (\y -> fromList $ map (fromLogDomain . (\x -> pdf (emission x) y)) states) values

-- | Runs forward-backward, then looks up the probability of ith latent
-- variable being in state s.
hmmExactMarginal :: Vector Double -> Matrix Double -> [Vector Double]
                    -> Int -> Int -> Double
hmmExactMarginal init trans obs =
  let
    marginals = forwardBackward init trans obs
    n = length obs
    lookup i s = marginals ! s ! i
  in
   lookup

-- | Runs forward-backward, then looks up the probability  of a sequence of states.
hmmExact :: Vector Double -> Matrix Double -> [Vector Double] -> [Int] -> Double
hmmExact init trans obs =
    let
        marginals = forwardBackward init trans obs
        n = length obs
        lookup i [] = if i == n then 1 else error "Dimension mismatch"
        lookup i (x:xs) = (marginals ! x ! i) * lookup (i+1) xs
    in
        lookup 0

-- | Produces marginal posteriors for all state variables using forward-backward.
-- In the resulting matrix the row index is state and column index is time.
forwardBackward :: Vector Double -> Matrix Double -> [Vector Double] -> Matrix Double
forwardBackward init trans obs = alphas * betas where
    (alphas,cs) = forward  init trans obs
    betas       = backward init trans obs cs

forward :: Vector Double -> Matrix Double -> [Vector Double] -> (Matrix Double, Vector Double)
forward init trans obs = (fromColumns as, fromList cs) where
    run p [] = ([],[])
    run p (w:ws) = (a:as, c:cs) where
        a' = w * (trans #> p)
        a  = scale c a'
        c  = 1 / norm_1 a'
        (as,cs) = run a ws
    (as,cs) = run init obs

backward :: Vector Double -> Matrix Double -> [Vector Double] -> Vector Double -> Matrix Double
backward init trans obs cs = fromColumns bs where
    run :: Vector Double -> [Vector Double] -> [Double] -> [Vector Double]
    run p [] [] = []
    run p (w:ws) (c:cs) = p:bs where
        b  = scale c (t #> (w * p))
        bs = run b ws cs
    t = tr trans
    bs = reverse $ run (cmap (const 1) init) (reverse obs) (reverse (toList cs))
    n = length obs
