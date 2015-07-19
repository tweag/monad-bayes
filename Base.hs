
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Base where

import System.Random (StdGen, Random)
import Control.Monad (liftM)
import qualified Data.Random as Ext

-- | Numeric values of probability and probability density.
-- This extra level of indirection is created mostly to allow seamless
-- migration between using probabilities and log-probabilities.
newtype Prob = Prob {toDouble :: Double}
    deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Floating, Random, Ext.Distribution Ext.StdUniform)
prob :: Double -> Prob
prob = Prob
toLog :: Prob -> Double
toLog = log . toDouble

-- | Generic data structures that can be sampled from.
-- It is tied to 'StdGen' at the moment, but perhaps a more generic constraint should be used here.
class Sampleable d where
    sample :: StdGen -> d a -> a

-- | Generic data structures that can provide likelihood scores.
class Scoreable d where
    score :: Eq a => d a -> a -> Prob

-- | An interface for constructing discrete probability distributions.
class DiscreteDist d where
    categorical :: [(a,Prob)] -> d a

    certainly :: a -> d a
    certainly x = categorical [(x,1)]
    uniform :: [a] -> d a
    uniform = categorical . map (,1)
    bernoulli :: Prob -> d Bool
    bernoulli p = categorical [(True,p), (False,1-p)]
    --binomial :: Int -> Prob -> m Int
    --multinomial
    --poisson :: Double -> m Int

    choice :: Monad d => Prob -> d a -> d a -> d a
    choice p x y = categorical [(True, p), (False, 1-p)] >>=
                   \c -> if c then x else y

-- | An interface for probabilistic programs that can include continuous distributions.
class ContinuousDist d where
    normal :: Double -> Double -> d Double
    --mvnormal :: Vector Double -> Matrix Double -> d (Vector Double)
    gamma :: Double -> Double -> d Double
    --invgamma
    beta :: Double -> Double -> d Double
    exponential :: Double -> d Double

-- | An interface for probabilistic programs that allow local conditioning.
class Conditional d where
    condition :: (a -> Prob) -> d a -> d a

-- | An interface for representation that can recover the prior and likelihood separately.
class Bayesian d where
    -- | Prior distribution with associated likelihood scores.
    prior :: d a -> d (a,Prob)

    -- | Equivalent to 'fmap' 'fst' . 'prior'
    prior' :: d a -> d a
--    prior' = fmap fst . prior

-- | Representations that can accommodate external samplers.
class Sampler d where
    sampler :: Sampleable s => s a -> d a

-------------------------------------------------------
--Conditioning helpers

-- | Noisy observe conditioning, similar to Anglican.
-- observe d f x is roughly equivalent to Anglican [observe f x].
observe :: (Eq b, Scoreable s, Conditional d) =>
                d a -> (a -> s b) -> b -> d a
observe d noise value =
    condition (\x -> score (noise x) value) d
