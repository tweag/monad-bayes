{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts
 #-}

module Sampler where

import System.Random
import Control.Monad (liftM2)

import Base

-- | A random sampler using `StdGen` as a source of randomness.
-- It uses `split` instead of passing the modified generator,
-- which makes `Sampler` lazy and potentially parallel.
-- Can be used for probabilistic computation, but not with conditioning,
-- unless transformed.
newtype Sampler a = Sampler (StdGen -> a)
    deriving (Functor)

instance Applicative Sampler where
    pure = return
    (<*>) = liftM2 ($)

instance Monad Sampler where
    return = Sampler . const
    Sampler d >>= f = Sampler $
                       \g -> let
                           x          = d g1
                           Sampler y  = f x
                           (g1,g2)    = split g
         in
           y g2

--TODO: fix this
instance MonadDist Sampler where
    categorical = undefined
    normal = undefined
    gamma = undefined
    beta = undefined


