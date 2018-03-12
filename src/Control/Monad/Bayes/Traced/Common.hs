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

type Trace = [Double]

mhTrans' :: MonadSample m => Weighted (FreeSampler Identity) a -> (Trace, a) -> m (Trace, a)
mhTrans' m (us,a) = do
  let (_, p) = runIdentity $ runWeighted $ Weighted.hoist (withRandomness us) m
  us' <- do
    let n = length us
    i <- categorical $ V.replicate n (1 / fromIntegral n)
    u' <- random
    let (xs, _:ys) = splitAt i us
    return $ xs ++ (u':ys)
  ((b, q), vs) <- runWith us' (runWeighted m)
  let ratio = (exp . ln) $ min 1 (q * fromIntegral (length vs) / p * fromIntegral (length us))
  accept <- bernoulli ratio
  return $ if accept then (vs,b) else (us,a)

mhTrans :: MonadSample m => Weighted (FreeSampler m) a -> (Trace, a) -> m (Trace, a)
mhTrans m (us,a) = do
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
  return $ if accept then (vs,b) else (us,a)
