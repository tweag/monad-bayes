-- |
-- Module      : Control.Monad.Bayes.Traced.Common
-- Description : Numeric code for Trace MCMC
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Common
  ( Trace,
    singleton,
    output,
    scored,
    bind,
    mhTrans,
    mhTrans',
  )
where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Free as FreeSampler
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Numeric.Log (Log, ln)
import Statistics.Distribution.DiscreteUniform (discreteUniformAB)

-- | Collection of random variables sampled during the program's execution.
data Trace a
  = Trace
      { -- | Sequence of random variables sampled during the program's execution.
        variables :: [Double],
        -- |
        output :: a,
        -- | The probability of observing this particular sequence.
        density :: Log Double
      }

instance Functor Trace where
  fmap f t = t {output = f (output t)}

instance Applicative Trace where
  pure x = Trace {variables = [], output = x, density = 1}
  tf <*> tx =
    Trace
      { variables = variables tf ++ variables tx,
        output = output tf (output tx),
        density = density tf * density tx
      }

instance Monad Trace where
  t >>= f =
    let t' = f (output t)
     in t' {variables = variables t ++ variables t', density = density t * density t'}

singleton :: Double -> Trace Double
singleton u = Trace {variables = [u], output = u, density = 1}

scored :: Log Double -> Trace ()
scored w = Trace {variables = [], output = (), density = w}

bind :: Monad m => m (Trace a) -> (a -> m (Trace b)) -> m (Trace b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  return $ t2 {variables = variables t1 ++ variables t2, density = density t1 * density t2}

-- | a proposal distribution over randomness
singleVariableProposal :: MonadSample m => [Double] -> m [Double]
singleVariableProposal us = do
  let n = length us
  i <- discrete $ discreteUniformAB 0 (n -1)
  u' <- random
  case splitAt i us of
    (xs, _ : ys) -> return $ xs ++ (u' : ys)
    _ -> error "impossible"

-- | A single Metropolis-corrected transition of single-site Trace MCMC
-- | with a proposal distribution p([x_j]^i+1 | [x_j]^i). The x_j correspond
-- | to the randomness of the sample, defined over the domain [0, 1].
mhTransWithProposal :: MonadSample m => ([Double] -> m [Double]) -> Weighted (FreeSampler m) a -> Trace a -> m (Trace a)
mhTransWithProposal proposal m t@Trace {variables = us, density = p} = do
  let n = length us
  us' <- proposal us
  ((b, q), vs) <- runWriterT $ runWeighted $ Weighted.hoist (WriterT . withPartialRandomness us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  return $ if accept then Trace vs b q else t

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTrans :: MonadSample m => Weighted (FreeSampler m) a -> Trace a -> m (Trace a)
mhTrans = mhTransWithProposal singleVariableProposal

-- | A variant of 'mhTrans' with an external sampling monad.
mhTrans' :: MonadSample m => Weighted (FreeSampler Identity) a -> Trace a -> m (Trace a)
mhTrans' m = mhTrans (Weighted.hoist (FreeSampler.hoist (return . runIdentity)) m)
