{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments             #-}

{-# OPTIONS_GHC -Wall                   #-}

module Essence (
  mhRunGeometric,
  MonadDistribution(..),
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
import Statistics.Distribution.Uniform (uniformDistr)
import Statistics.Distribution.Normal (normalDistr)
import Numeric.Log (Log(..))
import System.Random.Stateful (StatefulGen, mkStdGen, newIOGenM, uniformDouble01M, uniformRM, split)
import qualified System.Random.MWC.Distributions as MWC

import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)


import qualified Data.Random as R
import System.Random.Stateful (setStdGen, newStdGen)

-- | Draw from a continuous distribution using the inverse cumulative density
-- function.
draw :: (ContDistr d, MonadDistribution m) => d -> m Double
draw d = fmap (quantile d) random

class Monad m => MonadDistribution m where
  random :: m Double
  uniform :: Double -> Double -> m Double
  uniform a b = draw (uniformDistr a b)
  bernoulli :: Double -> m Bool
  bernoulli p = fmap (< p) random

instance MonadDistribution m => MonadDistribution (StateT s m) where
  random = lift random
  bernoulli = lift . bernoulli

instance (Monoid w, MonadDistribution m) => MonadDistribution (WriterT w m) where
  random = lift random
  bernoulli = lift . bernoulli

geometric :: MonadDistribution m => m Int
geometric = do
  x <- random
  if x < 0.2
    then return 1
    else do y <- geometric
            return $ 1 + y

-- | Random sampling functor.
newtype SamF a = Random (Double -> a) deriving (Functor)

-- | Free monad transformer over random sampling.
--
-- Uses the Church-encoded version of the free monad for efficiency.
newtype Density m a = Density {runDensity :: FT SamF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance MonadFree SamF (Density m) where
  wrap = Density . wrap . fmap runDensity

instance Monad m => MonadDistribution (Density m) where
  random = Density $ liftF (Random id)

-- | Hoist 'Density' through a monad transform.
hoistD :: (Monad m, Monad n) => (forall x. m x -> n x) -> Density m a -> Density n a
hoistD f (Density m) = Density (hoistFT f m)

-- | Execute computation with supplied values for random choices.
-- | Execute computation with supplied values for a subset of random choices.
-- Return the output value and a record of all random choices used, whether
-- taken as input or drawn using the transformed monad.
density :: MonadDistribution m => [Double] -> Density m a -> m (a, [Double])
density randomness (Density m) =
  runWriterT $ evalStateT (iterTM f $ hoistFT lift m) randomness
  where
    f (Random k) = do
      -- This block runs in StateT [Double] (WriterT [Double]) m.
      -- StateT propagates consumed randomness while WriterT records
      -- randomness used, whether old or new.
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

-- | Obtain an explicit value of the likelihood for a given value.
weighted :: Weighted m a -> m (a, Log Double)
weighted (Weighted m) = runStateT m 1

-- | Compute the sample and discard the weight.
--
-- This operation introduces bias.
unweighted :: Functor m => Weighted m a -> m a
unweighted = fmap fst . weighted

-- | Tracing monad that records random choices made in the program.
data Traced m a = Traced
  { -- | Run the program with a modified trace.
    model :: Weighted (Density Identity) a,
    -- | Record trace and output.
    traceDist :: m (Trace a)
  }

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

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps.
mh :: (StatefulGen g m, MonadReader g m) =>
       MonadDistribution m => Int -> Traced m a -> m [a]
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
  bernoulli = lift . bernoulli

-- | A variant of 'mhTrans' with an external sampling monad.
mhTrans' :: (StatefulGen g m, MonadReader g m) =>
             MonadDistribution m => Weighted (Density Identity) a -> Trace a -> m (Trace a)
mhTrans' m = mhTrans (hoistW (hoistD (return . runIdentity)) m)

-- | Apply a transformation to the transformed monad.
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
  accept <- bernoulli ratio
  return if accept then (Trace vs b q) else t

mhRunGeometric :: IO [Int]
mhRunGeometric = do
  setStdGen (mkStdGen 43)
  g <- newStdGen
  let (g1, g2) = split g
  stdGen1 <- newIOGenM g1
  stdGen2 <- newIOGenM g2
  runReaderT (unweighted $ (runReaderT (mh 1000 geometric) stdGen1)) stdGen2

instance StatefulGen g m => MonadDistribution (ReaderT g m) where
  random = ReaderT uniformDouble01M
  uniform a b = ReaderT $ uniformRM (a, b)
  bernoulli p = ReaderT $ MWC.bernoulli p

singleObs :: (MonadDistribution m, MonadFactor m) => m Double
singleObs = do
    mu <- normal 0.0 1.0
    factor $ normalPdf mu 1.0 4.0
    return mu

-- | Monads that can score different execution paths.
class Monad m => MonadFactor m where
  -- | Record a likelihood.
  factor ::
    -- | likelihood of the execution path
    Log Double ->
    m ()

instance MonadFactor m => MonadFactor (ReaderT r m) where
  factor = lift . factor

scored :: Log Double -> Trace ()
scored w = Trace {variables = [], output = (), probDensity = w}

instance MonadFactor m => MonadFactor (Traced m) where
  factor w = Traced (factor w) (factor w >> pure (scored w))

instance Monad m => MonadFactor (Weighted m) where
  factor w = Weighted (modify (* w))

-- | Probability density function of the normal distribution.
normalPdf ::
  -- | mean μ
  Double ->
  -- | standard deviation σ
  Double ->
  -- | sample x
  Double ->
  -- | relative likelihood of observing sample x in \(\mathcal{N}(\mu, \sigma^2)\)
  Log Double
normalPdf mu sigma x = Exp $ logDensity (normalDistr mu sigma) x

normal :: MonadDistribution m => Double -> Double -> m Double
normal m s = draw (normalDistr m s)

mhRunNormal :: IO [Double]
mhRunNormal= do
  setStdGen (mkStdGen 43)
  g <- newStdGen
  let (g1, g2) = split g
  stdGen1 <- newIOGenM g1
  stdGen2 <- newIOGenM g2
  runReaderT (unweighted $ (runReaderT (mh 1000 singleObs) stdGen1)) stdGen2
