{-# LANGUAGE
  GADTs,
  TupleSections,
  ScopedTypeVariables,
  MultiParamTypeClasses,
  FlexibleInstances
 #-}

module Dist where

import System.Random
import Control.Applicative (Applicative, pure, (<*>))
import Control.Arrow (first, second)
import Control.Monad (liftM, liftM2)

import Base
import Explicit hiding (djoin)
import Sampler (external, StdSampler)

-- | A symbolic representation of a probabilistic program which basically remembers all applications of 'return' and '>>='.
-- Formally a free model for a probability monad.
-- Additional constructors are Primitive and Conditional.
data Dist a where
    -- One element degenerate distribution.
    Return      :: a -> Dist a
    -- Marginalization.
    Bind        :: Dist b -> (b -> Dist a) -> Dist a
    -- Primitive that can be sampled from.
    Primitive   :: (Sampleable d) => d a -> Dist a
    -- Posterior from likelihood and prior.
    Conditional :: (a -> Prob) -> Dist a -> Dist a

instance Functor Dist where
    fmap  = liftM

instance Applicative Dist where
    pure  = return
    (<*>) = liftM2 ($)

instance Monad Dist where
    return = Return
    (>>=)  = Bind

instance Dirac a Dist where
    dirac = return

instance Bernoulli Dist where
    bernoulli p = Primitive (bernoulli p :: StdSampler Bool)

instance UniformD a Dist where
    uniformd = Primitive . (uniformd :: [a] -> StdSampler a)

instance Categorical a Dist where
    categorical = Primitive . (categorical :: [(a,Prob)] -> StdSampler a)

instance Normal Dist where
    normal m s     = Primitive (normal m s :: StdSampler Double)

instance UniformC Dist where
    uniformc a b = Primitive (uniformc a b :: StdSampler Double)

instance Exponential Dist where
    exponential l = Primitive (exponential l :: StdSampler Double)

instance Gamma Dist where
    gamma a b = Primitive (gamma a b :: StdSampler Double)

instance Beta Dist where
    beta a b = Primitive (beta a b :: StdSampler Double)



instance Conditional Dist where
    condition c d = Conditional c d

instance Sampler Dist where
    sampler = Primitive

instance Bayesian Dist where
    prior (Conditional c d) = do
        (x,s) <- prior d
        return (x, s * c x)
    --Prior is only extracted from the outer distribution.
    prior (Bind d f) = do
        (x,p) <- prior d
        y     <- f x
        return (y,p)
    -- Non-recursive cases are not conditional, so they just get score 1.
    prior d = fmap (,1) d

    prior' (Conditional c d) = prior' d
    prior' (Bind d f)        = prior' d >>= f
    prior' d = d


instance Sampleable Dist where
    sample g (Return x)     = x
    sample g (Primitive d)     = sample g d
    sample g (Bind d f)        = sample g1 $ f $ sample g2 d where
        (g1, g2) = split g
    sample g (Conditional c d) = error "Attempted to sample from a conditional distribution."


