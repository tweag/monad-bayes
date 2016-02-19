{-# LANGUAGE
  TupleSections,
  FlexibleInstances
 #-}


module Base where

import Data.Number.LogFloat
import Numeric.SpecFunctions
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy

-- | Monads for building generative probabilistic models.
-- The class does not specify any conditioning primitives.
-- For better granularity discrete and continuous distributions could be separated.
class Monad m => MonadDist m where
    -- | Categorical distribution, weights need not be normalized.
    categorical :: Foldable t => t (a,LogFloat) -> m a
    -- | Normal distribution parameterized by mean and standard deviation.
    normal :: Double -> Double -> m Double
    -- | Gamma distribution parameterized by shape and rate.
    gamma :: Double -> Double -> m Double
    -- | Beta distribution.
    beta :: Double -> Double -> m Double


    -- | Bernoulli distribution.
    bernoulli :: LogFloat -> m Bool
    bernoulli p = categorical [(True,p), (False,1-p)]
    -- | Binomial distribution. Returns the number of successes.
    binomial :: Int -> LogFloat -> m Int
    binomial n p = categorical $ map (\k -> (k, mass k)) [0..n] where
                     mass k = logFloat (n `choose` k) * (p `pow` k') * ((1-p) `pow` (n'-k')) where
                                                  n' = fromIntegral n
                                                  k' = fromIntegral k
    -- | Geometric distribution starting at 0.
    geometric :: LogFloat -> m Int
    geometric p = categorical $ map (\k -> (k, p * q `pow` (fromIntegral k))) [0..] where
                             q = 1 - p
    -- | Poisson distribution.
    poisson :: LogFloat -> m Int
    poisson p = categorical $ map (\k -> (k, mass k)) [0..] where
                             mass k = c * (p `pow` (fromIntegral k)) / (factorial k)
                             factorial k = logToLogFloat (logFactorial k)
                             c = logToLogFloat (- fromLogFloat p) -- exp (-p)

    -- | Exponential distribution parameterized by rate.
    exponential :: Double -> m Double
    exponential rate = gamma 1 (1 / rate)

-- | Probability monads that allow conditioning.
-- Both soft and hard conditions are allowed.
class MonadDist m => MonadBayes m where
    -- | Hard conditioning on an arbitrary predicate.
    -- By default implemented in terms of `fail`.
    condition :: Bool -> m ()
    condition b = if b then return () else fail "rejected"

    -- | Soft conditioning with an arbitrary factor, as found in factor graphs.
    -- Factor should be positive, otherwise `condition` should be used.
    factor :: LogFloat -> m ()


----------------------------------------------------------------------------
-- Instances


-- MaybeT leaves the forward computation to the transformed monad,
-- while handling hard conditioning by Nothing.
-- Soft conditioning is not defined.
instance MonadDist m => MonadDist (MaybeT m) where
    categorical = lift . categorical
    normal m s  = lift (normal m s)
    gamma a b   = lift (gamma a b)
    beta a b    = lift (beta a b)

instance MonadDist m => MonadBayes (MaybeT m) where
    factor = error "MaybeT does not support soft conditioning"



-- StateT leaves the forward computation to the transformed monad.
-- If the state is a weight, it is used to handle soft conditioning.
instance MonadDist m => MonadDist (StateT s m) where
    categorical = lift . categorical
    normal m s  = lift (normal m s)
    gamma a b   = lift (gamma a b)
    beta a b    = lift (beta a b)

instance MonadDist m => MonadBayes (StateT LogFloat m) where
    factor w = modify (* w)
