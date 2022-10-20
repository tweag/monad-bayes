{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Common
-- Description : Numeric code for Trace MCMC
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Common
  ( Trace (..),
    singleton,
    scored,
    bind,
    algo1,
    algo2,
    mhTransGenPersp2,
    mhTrans,
    mhTransWithBool,
    mhTransFree,
    mhTrans',
    burnIn,
    MHResult (..),
  )
where

import Control.Monad.Bayes.Class
  ( MonadDistribution (bernoulli, random),
    discrete,
  )
import qualified Control.Monad.Bayes.Density.Free as Free
import qualified Control.Monad.Bayes.Density.State as State
import Control.Monad.Bayes.Weighted as Weighted
  ( Weighted,
    hoist,
    weighted,
  )
import Control.Monad.Writer (WriterT (WriterT, runWriterT))
import Data.Functor.Identity (Identity (runIdentity))
import Numeric.Log (Log, ln)
import Statistics.Distribution.DiscreteUniform (discreteUniformAB)

import qualified Debug.Trace as D

data MHResult a = MHResult
  { success :: Bool,
    trace :: Trace a
  }

-- | Collection of random variables sampler during the program's execution.
data Trace a = Trace
  { -- | Sequence of random variables sampler during the program's execution.
    variables :: [Double],
    --
    output :: a,
    -- | The probability of observing this particular sequence.
    probDensity :: Log Double
  }

instance Functor Trace where
  fmap f t = t {output = f (output t)}

instance Applicative Trace where
  pure x = Trace {variables = [], output = x, probDensity = 1}
  tf <*> tx =
    Trace
      { variables = variables tf ++ variables tx,
        output = output tf (output tx),
        probDensity = probDensity tf * probDensity tx
      }

instance Monad Trace where
  t >>= f =
    let t' = f (output t)
     in t' {variables = variables t ++ variables t', probDensity = probDensity t * probDensity t'}

singleton :: Double -> Trace Double
singleton u = Trace {variables = [u], output = u, probDensity = 1}

scored :: Log Double -> Trace ()
scored w = Trace {variables = [], output = (), probDensity = w}

bind :: Monad m => m (Trace a) -> (a -> m (Trace b)) -> m (Trace b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  return $ t2 {variables = variables t1 ++ variables t2, probDensity = probDensity t1 * probDensity t2}

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTrans :: MonadDistribution m => (Weighted (State.Density m)) a -> Trace a -> m (Trace a)
mhTrans m t@Trace {variables = us, probDensity = p} = do
  let n = length us
  us' <- do
    i <- discrete $ discreteUniformAB 0 (n - 1)
    u' <- random
    case splitAt i us of
      (xs, _ : ys) -> return $ xs ++ (u' : ys)
      _ -> error "impossible"
  ((b, q), vs) <- State.density (weighted m) us'
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  return $ if accept then Trace vs b q else t

_mhTransA1 :: MonadDistribution m => (Weighted (State.Density m)) a -> Trace a -> m (Trace a)
_mhTransA1 m Trace {variables = us, output = a, probDensity = p} = do
  ((_, r), ws) <- State.density (weighted m) us
  let n = length us
  us' <- do
    i <- discrete $ discreteUniformAB 0 (n - 1)
    u' <- random
    case splitAt i us of
      (xs, _ : ys) -> return $ xs ++ (u' : ys)
      _ -> error "impossible"
  ((b, q), vs) <- State.density (weighted m) us'
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (r * fromIntegral (length ws)))
  accept <- bernoulli ratio
  return $ if accept then Trace vs b q else Trace ws a p

mhTransFree :: MonadDistribution m => Weighted (Free.Density m) a -> Trace a -> m (Trace a)
mhTransFree m t = trace <$> mhTransWithBool m t

algo1 :: Show a => Show b => (MonadDistribution m, Fractional t) =>
         (a, c) -> (a -> m b) -> ((a, b) -> (a, b)) -> (t -> Double) -> ((a, b) -> t) -> m (a, b)
algo1 (currXi0, _) condDistXiMinus0GivenXi0 phi a rho = do
  xiMinus0 <- condDistXiMinus0GivenXi0 currXi0
  let xi = (currXi0, xiMinus0)
  let alpha = a $ (rho . phi) xi / rho xi
  u <- random
  if u < alpha
    then return $ phi xi
    else return xi

algo2 :: Show a => Show b => (MonadDistribution m, Fractional t) =>
         (a, c) -> (a -> m b) -> ((a, b) -> (a, b)) -> (t -> Double) -> ((a, b) -> m t) -> m (a, b)
algo2 (currXi0, _) condDistXiMinus0GivenXi0 phi a rho = do
  xiMinus0 <- condDistXiMinus0GivenXi0 currXi0
  let xi = (currXi0, xiMinus0)
  u <- rho xi
  v <- rho . phi $ xi
  let alpha = a $ v / u
  w <- random
  if w < alpha
    then return $ phi xi
    else return xi

getXi_0 :: MonadDistribution m =>
            [Double] -> m [Double]
getXi_0 us = do
  let n = length us
  i <- discrete $ discreteUniformAB 0 (n - 1)
  u' <- random
  case splitAt i us of
    (xs, _ : ys) -> return $ xs ++ (u' : ys)
    _ -> error "impossible"

tau :: MonadDistribution m =>
       Weighted (Free.Density m) a -> [Double] -> m (Log Double)
tau m us = do
  ((_, q), vs) <- runWriterT $ weighted $ Weighted.hoist (WriterT . Free.density us) m
  return $ q / fromIntegral (length vs)


mhTransGenPersp2 :: MonadDistribution m =>
                    Weighted (Free.Density m) a ->
                    ([Double], [Double]) ->
                    m ([Double], [Double])
mhTransGenPersp2 m (xi0, xi_0) =
  algo2 (xi0, xi_0) getXi_0
        (\(x, y) -> (y, x)) (min 1.0) (fmap (exp . ln) . ((tau m) <$> fst))

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTransWithBool :: MonadDistribution m => Weighted (Free.Density m) a -> Trace a -> m (MHResult a)
mhTransWithBool m t@Trace {variables = us, probDensity = p} = do
  let n = length us
  us' <- do
    i <- discrete $ discreteUniformAB 0 (n - 1)
    u' <- random
    case splitAt i us of
      (xs, _ : ys) -> return $ xs ++ (u' : ys)
      _ -> error "impossible"
  ((b, q), vs) <- runWriterT $ weighted $ Weighted.hoist (WriterT . Free.density us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  return if accept then MHResult True (Trace vs b q) else MHResult False t

-- | A variant of 'mhTrans' with an external sampling monad.
mhTrans' :: MonadDistribution m => Weighted (Free.Density Identity) a -> Trace a -> m (Trace a)
mhTrans' m = mhTransFree (Weighted.hoist (Free.hoist (return . runIdentity)) m)

-- | burn in an MCMC chain for n steps (which amounts to dropping samples of the end of the list)
burnIn :: Functor m => Int -> m [a] -> m [a]
burnIn n = fmap dropEnd
  where
    dropEnd ls = let len = length ls in take (len - n) ls
