{-|
Module      : Control.Monad.Bayes.Traced.Common
Description : Numeric code for Trace MCMC
Copyright   : (c) Adam Scibior, 2015-2020
License     : MIT
Maintainer  : leonhard.markert@tweag.io
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Traced.Common (
  Trace,
  singleton,
  output,
  scored,
  bind,
  mhTrans,
  mhTrans'
) where

import Control.Monad.Trans.Writer
import qualified Data.Vector as V
import Data.Functor.Identity

import Numeric.Log (Log, ln)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Free as FreeSampler

data Trace a =
  Trace {
    variables :: [Double],
    output :: a,
    density :: Log Double
  }

instance Functor Trace where
  fmap f t = t {output = f (output t)}

instance Applicative Trace where
  pure x = Trace {variables = [], output = x, density = 1}
  tf <*> tx = Trace {variables = variables tf ++ variables tx, output = output tf (output tx), density = density tf * density tx}

instance Monad Trace where
  t >>= f =
    let t' = f (output t) in
    t' {variables = variables t ++ variables t', density = density t * density t'}

singleton :: Double -> Trace Double
singleton u = Trace {variables = [u], output = u, density = 1}

scored :: Log Double -> Trace ()
scored w = Trace {variables = [], output = (), density = w}

bind :: Monad m => m (Trace a) -> (a -> m (Trace b)) -> m (Trace b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  return $ t2 {variables = variables t1 ++ variables t2, density = density t1 * density t2}

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTrans :: MonadSample m => Weighted (FreeSampler m) a -> Trace a -> m (Trace a)
mhTrans m t = do
  let us = variables t
      p = density t
  us' <- do
    let n = length us
    i <- categorical $ V.replicate n (1 / fromIntegral n)
    u' <- random
    let (xs, _:ys) = splitAt i us
    return $ xs ++ (u':ys)
  ((b, q), vs) <- runWriterT $ runWeighted $ Weighted.hoist (WriterT . withPartialRandomness us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral (length us) / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  return $ if accept then Trace vs b q else t

-- | A variant of 'mhTrans' with an external sampling monad.
mhTrans' :: MonadSample m => Weighted (FreeSampler Identity) a -> Trace a -> m (Trace a)
mhTrans' m = mhTrans (Weighted.hoist (FreeSampler.hoist (return . runIdentity)) m)
