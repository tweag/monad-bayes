{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments             #-}

{-# OPTIONS_GHC -Wall                   #-}

module Gist (
  mhRunNormal,
  ) where

import Control.Applicative (liftA2)
import Control.Monad.RWS (MonadIO, MonadTrans, lift)
import Control.Monad.State (evalStateT, modify)
import Control.Monad.Trans.Free.Church (FT, MonadFree (..), hoistFT, iterTM, liftF)
import Control.Monad.Writer (WriterT (..), tell)
import Control.Monad.State (StateT, runStateT, mapStateT, put, get)
import Control.Monad.Identity (IdentityT)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Reader.Class (MonadReader)

import Statistics.Distribution (ContDistr, quantile, logDensity)
import Statistics.Distribution.Normal (normalDistr)
import Numeric.Log (Log(..))
import System.Random.Stateful (StatefulGen, mkStdGen, newIOGenM, uniformDouble01M, split)

import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)

import qualified Data.Random as R
import System.Random.Stateful (setStdGen, newStdGen)


class Monad m => MonadDistribution m where
  random :: m Double

instance MonadDistribution m => MonadDistribution (StateT s m) where
  random = lift random

instance (Monoid w, MonadDistribution m) => MonadDistribution (WriterT w m) where
  random = lift random

class Monad m => MonadFactor m where
  factor ::
    Log Double ->
    m ()

normalPdf :: Double -> Double -> Double -> Log Double
normalPdf mu sigma x = Exp $ logDensity (normalDistr mu sigma) x

draw :: (ContDistr d, MonadDistribution m) => d -> m Double
draw d = fmap (quantile d) random

normal :: MonadDistribution m => Double -> Double -> m Double
normal m s = draw (normalDistr m s)

singleObs :: (MonadDistribution m, MonadFactor m) => m Double
singleObs = do
    mu <- normal 0.0 1.0
    factor $ normalPdf mu 1.0 4.0
    return mu

newtype SamF a = Random (Double -> a) deriving (Functor)

newtype Density m a = Density {runDensity :: FT SamF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance MonadFree SamF (Density m) where
  wrap = Density . wrap . fmap runDensity

instance Monad m => MonadDistribution (Density m) where
  random = Density $ liftF (Random id)

hoistD :: (Monad m, Monad n) => (forall x. m x -> n x) -> Density m a -> Density n a
hoistD f (Density m) = Density (hoistFT f m)

density :: MonadDistribution m => [Double] -> Density m a -> m (a, [Double])
density randomness (Density m) =
  runWriterT $ evalStateT (iterTM f $ hoistFT lift m) randomness
  where
    f (Random k) = do
      xs <- get
      x <- case xs of
        [] -> random
        y : ys -> put ys >> return y
      tell [x]
      k x

data Trace a = Trace { variables :: [Double], output :: a, probDensity :: Log Double }

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

bind :: Monad m => m (Trace a) -> (a -> m (Trace b)) -> m (Trace b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  return $ t2 {variables = variables t1 ++ variables t2, probDensity = probDensity t1 * probDensity t2}

newtype Weighted m a = Weighted (StateT (Log Double) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadDistribution)

weighted :: Weighted m a -> m (a, Log Double)
weighted (Weighted m) = runStateT m 1

unweighted :: Functor m => Weighted m a -> m a
unweighted = fmap fst . weighted

data Traced m a = Traced { model :: Weighted (Density Identity) a, traceDist :: m (Trace a) }

instance Monad m => Functor (Traced m) where
  fmap f (Traced m d) = Traced (fmap f m) (fmap (fmap f) d)

instance Monad m => Applicative (Traced m) where
  pure x = Traced (pure x) (pure (pure x))
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) (liftA2 (<*>) df dx)

instance Monad m => Monad (Traced m) where
  (Traced mx dx) >>= f = Traced my dy
    where
      my = mx >>= model . f
      dy = dx `bind` (traceDist . f)

instance MonadDistribution m => MonadDistribution (Traced m) where
  random = Traced random (fmap singleton random)

mh :: (StatefulGen g m, MonadReader g m) => MonadDistribution m => Int -> Traced m a -> m [a]
mh n (Traced m d) = fmap (map output . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) d
      | otherwise = do
        (x :| xs) <- f (k - 1)
        y <- mhTrans' m x
        return (y :| x : xs)

instance MonadDistribution m => MonadDistribution (IdentityT m) where
  random = lift random

mhTrans' :: (StatefulGen g m, MonadReader g m) =>
             MonadDistribution m => Weighted (Density Identity) a -> Trace a -> m (Trace a)
mhTrans' m = mhTrans (hoistW (hoistD (return . runIdentity)) m)

hoistW :: (forall x. m x -> n x) -> Weighted m a -> Weighted n a
hoistW t (Weighted m) = Weighted $ mapStateT t m

mhTrans :: (MonadReader g m, StatefulGen g m) =>
            MonadDistribution m => Weighted (Density m) a -> Trace a -> m (Trace a)
mhTrans m t@Trace {variables = us, probDensity = p} = do
  let n = length us
  us' <- do
    i <- R.sample $ R.uniform (0 :: Int) (n - 1)
    u' <- R.sample $ R.uniform 0.0 1.0
    case splitAt i us of
      (xs, _ : ys) -> return $ xs ++ (u' : ys)
      _ -> error "impossible"
  ((b, q), vs) <- runWriterT $ weighted $ hoistW (WriterT . density us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  u'' <- R.sample $ R.uniform 0.0 1.0
  let accept = u'' < ratio
  return if accept then (Trace vs b q) else t

instance StatefulGen g m => MonadDistribution (ReaderT g m) where
  random = ReaderT uniformDouble01M

instance MonadFactor m => MonadFactor (ReaderT r m) where
  factor = lift . factor

scored :: Log Double -> Trace ()
scored w = Trace {variables = [], output = (), probDensity = w}

instance MonadFactor m => MonadFactor (Traced m) where
  factor w = Traced (factor w) (factor w >> pure (scored w))

instance Monad m => MonadFactor (Weighted m) where
  factor w = Weighted (modify (* w))

mhRunNormal :: IO [Double]
mhRunNormal= do
  setStdGen (mkStdGen 43)
  g <- newStdGen
  let (g1, g2) = split g
  stdGen1 <- newIOGenM g1
  stdGen2 <- newIOGenM g2
  runReaderT (unweighted $ (runReaderT (mh 1000 singleObs) stdGen1)) stdGen2
