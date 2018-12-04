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
  scored,
  bind,
  withTrace,
  withTrace',
  importanceWithTrace,
  importanceWithTrace',
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

withVariables :: MonadSample m => Weighted (FreeSampler m) a -> [Double] ->
                 m (Trace a)
withVariables m vs = do
  ((a, p), vs') <- runWriterT $ runWeighted $ Weighted.hoist (WriterT . withPartialRandomness vs) m
  return $ Trace vs' a p

withVariables' :: MonadSample m => Weighted (FreeSampler Identity) a ->
                 [Double] -> m (Trace a)
withVariables' m vs = do
  ((a, p), vs') <- runWriterT $ runWeighted $ Weighted.hoist (WriterT . runWith vs) m
  return $ Trace vs' a p

withTrace :: MonadSample m => Weighted (FreeSampler m) a -> Trace b ->
             m (Trace a)
withTrace m t = withVariables m (variables t)

withTrace' :: MonadSample m => Weighted (FreeSampler Identity) a -> Trace b ->
             m (Trace a)
withTrace' m t = withVariables' m (variables t)

importanceWithTrace :: MonadSample m => Weighted (FreeSampler m) a -> Trace b ->
                       m (Trace a)
importanceWithTrace m t = do
  (Trace vsp a p) <- withVariables m vsq
  return $ Trace (importanceVs vsp) a (p / q)
  where
    vsq = variables t
    q = density t
    extendedVsQ vsp = vsq ++ (take (length vsp - length vsq) (repeat 1))
    importanceVs vsp = zipWith (/) vsp (extendedVsQ vsp)

importanceWithTrace' :: MonadSample m => Weighted (FreeSampler Identity) a ->
                        Trace b -> m (Trace a)
importanceWithTrace' m t = do
  (Trace vsp a p) <- withVariables' m vsq
  return $ Trace (importanceVs vsp) a (p / q)
  where
    vsq = variables t
    q = density t
    extendedVsQ vsp = vsq ++ (take (length vsp - length vsq) (repeat 1))
    importanceVs vsp = zipWith (/) vsp (extendedVsQ vsp)

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTrans :: MonadSample m => Weighted (FreeSampler m) a -> Trace a -> m (Trace a)
mhTrans m t = do
  let us = variables t
      a = output t
      p = density t
  us' <- do
    let n = length us
    i <- categorical $ V.replicate n (1 / fromIntegral n)
    u' <- random
    let (xs, _:ys) = splitAt i us
    return $ xs ++ (u':ys)
  ((b, q), vs) <- runWriterT $ runWeighted $ Weighted.hoist (WriterT . withPartialRandomness us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral (length vs) / p * fromIntegral (length us))
  accept <- bernoulli ratio
  return $ if accept then Trace vs b q else t

-- | A variant of 'mhTrans' with an external sampling monad.
mhTrans' :: MonadSample m => Weighted (FreeSampler Identity) a -> Trace a -> m (Trace a)
mhTrans' m = mhTrans (Weighted.hoist (FreeSampler.hoist (return . runIdentity)) m)
