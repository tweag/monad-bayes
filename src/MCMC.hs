{-# LANGUAGE
  TupleSections
 #-}

module MCMC (mh, pimh) where

import Base
import Dist
import SMC hiding (normalize)
import Importance

import Control.Monad (liftM)

-- | Metropolis-Hastings algorithm proposing values from the prior.
-- Produces an infinite list of subsequent samples from a Markov chain.
-- Note that this is not RandomDB, but rather the whole 'trace' is proposed
-- from the prior at each step.
mh :: (Functor d, Monad d, Bayesian d, Bernoulli d) =>
               d a -> d [a]
mh d =
    fmap (map fst) $ proposal >>= iterate where
        proposal = prior d
        iterate (x,s) = do
          (y,r) <- proposal
          accept <- bernoulli $ min 1 (r / s)
          let next = if accept then (y,r) else (x,s)
          rest  <- iterate next
          return $ next:rest

mh' :: (Functor d, Monad d, Bayesian d, Bernoulli d) =>
               Int -> d a -> d a
mh' n d = fmap (!! n) (mh d)

-- | The Particle Independent Metrpolis-Hastings algorithm.
-- Returns a chain of particle systems rather than a chain of individual samples.
pimh :: Int -> Dist a -> Dist [Samples a]
pimh n = mh . smc n

pimh' :: Int -> Int -> Dist a -> Dist a
pimh' k n = mh' k . smc' n
