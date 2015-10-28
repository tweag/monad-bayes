{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses
 #-}


module Base where

import System.Random (StdGen, Random)
import Control.Monad (liftM)
import qualified Data.Random as Ext

-- | Numeric values of probability and probability density.
-- This extra level of indirection is created mostly to allow seamless
-- migration between using probabilities and log-probabilities.
newtype Prob = Prob {toDouble :: Double}
    deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Floating, Random, Ext.Distribution Ext.StdUniform, Ext.Distribution Ext.Uniform)
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

-- | An interface for constructing Dirac distributions.
class Dirac a d where
    dirac :: a -> d a

-- | An interface for constructing Bernoulli distributions. The argument should be between 0 and 1.
class Bernoulli d where
    bernoulli :: Prob -> d Bool
    choice :: Monad d => Prob -> d a -> d a -> d a
    choice p x y = bernoulli p >>= \c -> if c then x else y

-- | An interface for constructing discrete uniform distributions. The argument should be a non-empty, finite list.
class UniformD a d where
    uniformd :: [a] -> d a

-- | An interface for constructing categorical distributions. The argument should be a non-empty, finite list of weighed elements. The weights need not sum to 1.
class Categorical a d where
    categorical :: [(a,Prob)] -> d a

-- | An interface for constructing normal distributions. The parameters are mean and standard deviation.
class Normal d where
    -- mean stddev
    normal :: Double -> Double -> d Double

-- | An interface for constructing continuous uniform distributions. The parameters are boundaries of the interval.
class UniformC d where
    uniformc :: Double -> Double -> d Double

-- | An interface for constructing exponential distributions. The parameter is rate.
class Exponential d where
    -- rate
    exponential :: Double -> d Double

-- | An interface for constructing gamma distributions. The parameters are shape and rate.
class Exponential d => Gamma d where
    -- shape rate
    gamma :: Double -> Double -> d Double

-- | An interface for constructing beta distributions. The parameters are alpha and beta.
class Beta d where
    -- alpha beta
    beta :: Double -> Double -> d Double

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
