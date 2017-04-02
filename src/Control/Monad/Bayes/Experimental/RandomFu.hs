{-|
Module      : Control.Monad.Bayes.Samplers.RandomFu
Description : Psuedo-random sampling monads based on random-fu package
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

Currently not included in the main monad-bayes package to reduce the number of dependencies.
Includes MonadDist instance for RVarT from random-fu.
-}

{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts,
  TypeFamilies
 #-}

module Control.Monad.Bayes.Samplers.RandomFu (
    SamplerLazy,
    sampleLazy
               ) where

import System.Random
import Control.Monad (liftM2)
import Data.Random

import Data.Random.Distribution
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Categorical

import Control.Monad.Bayes.Class


---------------------------------------------------
-- MonadDist instance for the RVar monad

type instance CustomReal (RVarT m) = Double

instance MonadDist (RVarT m) where
  discrete xs = rvarT $ fromWeightedList $ zip xs [0..]
  normal  m s = rvarT $ Normal m s
  gamma   a b = rvarT $ Gamma a (recip b)
  beta    a b = rvarT $ Beta a b
  uniform a b = rvarT $ Uniform a b
  -- could add implementations for other distributions as available in RVarT


--------------------------------------------
-- Lazy sampler

-- | A random sampler using `StdGen` as a source of randomness.
-- It uses `split` instead of passing the modified generator,
-- which makes lazy and potentially parallel,
-- but strictly speaking not associative.
newtype SamplerLazy a = SamplerLazy (StdGen -> a)
    deriving (Functor)

-- | Samples a value using the supplied RNG.
sampleLazy :: SamplerLazy a -> StdGen -> a
sampleLazy (SamplerLazy s) = s

instance Applicative SamplerLazy where
    pure = return
    (<*>) = liftM2 ($)

instance Monad SamplerLazy where
    return = SamplerLazy . const
    SamplerLazy d >>= f = SamplerLazy $
                       \g -> let
                           x          = d g1
                           SamplerLazy y  = f x
                           (g1,g2)    = split g
         in
           y g2

type instance CustomReal SamplerLazy = Double

instance MonadDist SamplerLazy where
  discrete xs = wrapper $ fromWeightedList $ zip xs [0..]
  normal m s  = wrapper $ Normal m s
  gamma a b   = wrapper $ Gamma a (recip b)
  beta a b    = wrapper $ Beta a b
  uniform a b = wrapper $ Uniform a b

-- | Wrapper for random-fu distributions.
wrapper :: Distribution d a => d a -> SamplerLazy a
wrapper = SamplerLazy . (fst .) . sampleState
