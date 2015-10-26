{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Dist where

import System.Random
import Data.Random.Distribution.Beta (Beta(Beta))
import Data.Random.Distribution.Exponential (Exponential(Exp))
import qualified Data.Random as Ext
import Control.Applicative (Applicative, pure, (<*>))
import Control.Arrow (first, second)
import Control.Monad (liftM, liftM2)

import Base
import Explicit hiding (djoin)
import Sampler (external)

-- | A symbolic representation of a probabilistic program which basically remembers all applications of 'return' and '>>='.
-- Formally a free model for a probability monad.
-- Additional constructors are Primitive and Conditional.
data Dist a where
    -- One element degenerate distribution.
    Return      :: a -> Dist a
    -- Application of a function to a random variable.
    Bind        :: Dist b -> (b -> Dist a) -> Dist a
    -- A primitive distribution that can be sampled from.
    Primitive   :: (Sampleable d) => d a -> Dist a
    -- A posterior distribution composed of a prior and a likelihood.
    Conditional :: (a -> Prob) -> Dist a -> Dist a

instance Functor Dist where
    fmap  = liftM

instance Applicative Dist where
    pure  = return
    (<*>) = liftM2 ($)

instance Monad Dist where
    return = Return
    (>>=)  = Bind

instance DiscreteDist Dist where
    categorical = Primitive . (categorical :: [(a,Prob)] -> Explicit a)

instance ContinuousDist Dist where
    normal m s     = external $ Ext.Normal m s
    gamma  k t     = external $ Ext.Gamma  k t
    beta   a b     = external $ Beta       a b
    exponential  l = external $ Exp        l

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


