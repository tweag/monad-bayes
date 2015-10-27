
{-# LANGUAGE TupleSections #-}

module Sampler where

import Base
import Explicit (Explicit)

import qualified Data.Random.Distribution.Beta as Beta
import qualified Data.Random.Distribution.Exponential as Exp
import qualified Data.Random.Distribution.Bernoulli as Bern
import qualified Data.Random.Distribution.Categorical as Cat
import qualified Data.Random as Ext
import Control.Arrow (first, second)
import Control.Applicative (Applicative, pure, (<*>))
import System.Random (StdGen, split)
import Data.Tuple

-- | A basic probability monad for sampling from tractable distributions.
-- Does not support conditioning.
newtype StdSampler a = StdSampler {run :: StdGen -> a}

instance Functor StdSampler where
    fmap f (StdSampler x) = StdSampler (f . x)

instance Applicative StdSampler where
    pure x = StdSampler (\_ -> x)
    (StdSampler f) <*> (StdSampler x) =
        StdSampler (uncurry ($) . first f . second x . split)

instance Monad StdSampler where
    return = pure
    (StdSampler x) >>= f =
      StdSampler (uncurry ($) . first (run . f . x) . split)

instance Dirac StdSampler where
    dirac = return

instance Bernoulli StdSampler where
    bernoulli p = external $ Bern.Bernoulli p

instance UniformD StdSampler where
    uniformd xs = categorical $ map (,1) xs

instance Categorical StdSampler where
    categorical xs = external $ Cat.fromWeightedList $ map swap xs

instance Normal StdSampler where
    normal m s    = external $ Ext.Normal m s

instance UniformC StdSampler where
    uniformc a b = external $ Ext.Uniform a b 

instance Exponential StdSampler where
    exponential l = external $ Exp.Exp    l

instance Gamma StdSampler where
    gamma  k t    = external $ Ext.Gamma  k t

instance Beta StdSampler where
    beta   a b    = external $ Beta.Beta   a b


instance Sampler StdSampler where
    sampler = StdSampler . flip sample


instance Sampleable StdSampler where
    sample = flip run

-- | A wrapper for externally defined distributions.
external :: (Ext.Distribution d a, Sampler s) => d a -> s a
external d = sampler $ StdSampler (fst . Ext.sampleState d)
