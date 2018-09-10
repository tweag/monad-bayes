{-|
Module      : Control.Monad.Bayes.Traced.Common
Description : Numeric code for Trace MCMC
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Traced.Common (
  Trace,
  singleton,
  output,
  bind,
  mhTrans,
  mhTrans'
) where

import Control.Monad.Trans.Writer
import qualified Data.Vector as V
import Data.Functor.Identity

import Numeric.Log (ln)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Free as FreeSampler

newtype Trace a = Trace ([Double], a)

instance Functor Trace where
  fmap f (Trace (us, x)) = Trace (us, f x)

instance Applicative Trace where
  pure x = Trace ([], x)
  (Trace (us, f)) <*> (Trace (vs, x)) = Trace (us ++ vs, f x)

instance Monad Trace where
  (Trace (us, x)) >>= f =
    let Trace (vs, y) = f x in
    Trace (us ++ vs, y)

singleton :: Double -> Trace Double
singleton u = Trace ([u], u)

output :: Trace a -> a
output (Trace (_, x)) = x

bind :: Monad m => m (Trace a) -> (a -> m (Trace b)) -> m (Trace b)
bind dx f = do
  (Trace (us, x)) <- dx
  (Trace (vs, y)) <- f x
  return $ Trace (us ++ vs, y)

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTrans :: MonadSample m => Weighted (FreeSampler m) a -> Trace a -> m (Trace a)
mhTrans m (Trace (us,a)) = do
  (_, p) <- runWeighted $ Weighted.hoist (withRandomness us) m
  us' <- do
    let n = length us
    i <- categorical $ V.replicate n (1 / fromIntegral n)
    u' <- random
    let (xs, _:ys) = splitAt i us
    return $ xs ++ (u':ys)
  ((b, q), vs) <- runWriterT $ runWeighted $ Weighted.hoist (WriterT . withPartialRandomness us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral (length vs) / p * fromIntegral (length us))
  accept <- bernoulli ratio
  return $ Trace $ if accept then (vs,b) else (us,a)

-- | A variant of 'mhTrans' with an external sampling monad.
mhTrans' :: MonadSample m => Weighted (FreeSampler Identity) a -> Trace a -> m (Trace a)
mhTrans' m = mhTrans (Weighted.hoist (FreeSampler.hoist (return . runIdentity)) m)
