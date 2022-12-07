{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -Wall              #-}

module Essence (mhRunGeometric) where

import Control.Applicative (liftA2)
import Control.Monad.RWS (MonadIO, MonadTrans, lift, when)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Free.Church (FT, MonadFree (..), hoistFT, iterTM, liftF)
import Control.Monad.Writer (WriterT (..), tell)
import Control.Monad.State (StateT, runStateT, mapStateT, put, get)
import Control.Monad.Identity (IdentityT)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT (..))

import Statistics.Distribution (ContDistr, DiscreteDistr, quantile, probability)
import Statistics.Distribution.Uniform (uniformDistr)
import Statistics.Distribution.DiscreteUniform (discreteUniformAB)
import Numeric.Log (Log(..))
import System.Random.Stateful (IOGenM (..), StatefulGen, StdGen, mkStdGen, newIOGenM, uniformDouble01M, uniformRM)
import qualified System.Random.MWC.Distributions as MWC

import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)


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
hoist :: (Monad m, Monad n) => (forall x. m x -> n x) -> Density m a -> Density n a
hoist f (Density m) = Density (hoistFT f m)

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

newtype Weighted m a = Weighted (StateT (Log Double) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadDistribution)

-- | Draw from a continuous distribution using the inverse cumulative density
-- function.
draw :: (ContDistr d, MonadDistribution m) => d -> m Double
draw d = fmap (quantile d) random

-- | Draw from a discrete distributions using the probability mass function.
discrete :: (DiscreteDistr d, MonadDistribution m) => d -> m Int
discrete = fromPMF . probability

-- | Draw from a discrete distribution using a sequence of draws from
-- Bernoulli.
fromPMF :: MonadDistribution m => (Int -> Double) -> m Int
fromPMF p = f 0 1
  where
    f i r = do
      when (r < 0) $ error "fromPMF: total PMF above 1"
      let q = p i
      when (q < 0 || q > 1) $ error "fromPMF: invalid probability value"
      b <- bernoulli (q / r)
      if b then pure i else f (i + 1) (r - q)

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
mh :: MonadDistribution m => Int -> Traced m a -> m [a]
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
mhTrans' :: MonadDistribution m => Weighted (Density Identity) a -> Trace a -> m (Trace a)
mhTrans' m = mhTransFree (hoistW (hoist (return . runIdentity)) m)

-- | Apply a transformation to the transformed monad.
hoistW :: (forall x. m x -> n x) -> Weighted m a -> Weighted n a
hoistW t (Weighted m) = Weighted $ mapStateT t m

singleton :: Double -> Trace Double
singleton u = Trace {variables = [u], output = u, probDensity = 1}

bind :: Monad m => m (Trace a) -> (a -> m (Trace b)) -> m (Trace b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  return $ t2 {variables = variables t1 ++ variables t2, probDensity = probDensity t1 * probDensity t2}

mhTransFree :: MonadDistribution m => Weighted (Density m) a -> Trace a -> m (Trace a)
mhTransFree m t = trace <$> mhTransWithBool m t

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTransWithBool :: MonadDistribution m => Weighted (Density m) a -> Trace a -> m (MHResult a)
mhTransWithBool m t@Trace {variables = us, probDensity = p} = do
  let n = length us
  us' <- do
    i <- discrete $ discreteUniformAB 0 (n - 1)
    u' <- random
    case splitAt i us of
      (xs, _ : ys) -> return $ xs ++ (u' : ys)
      _ -> error "impossible"
  ((b, q), vs) <- runWriterT $ weighted $ hoistW (WriterT . density us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  return if accept then MHResult True (Trace vs b q) else MHResult False t

data MHResult a = MHResult
  { success :: Bool,
    trace :: Trace a
  }

newtype Sampler g m a = Sampler (ReaderT g m a) deriving (Functor, Applicative, Monad, MonadIO)

type SamplerIO = Sampler (IOGenM StdGen) IO

instance StatefulGen g m => MonadDistribution (Sampler g m) where
  random = Sampler (ReaderT uniformDouble01M)
  uniform a b = Sampler (ReaderT $ uniformRM (a, b))
  bernoulli p = Sampler (ReaderT $ MWC.bernoulli p)

sampleWith :: StatefulGen g m => Sampler g m a -> g -> m a
sampleWith (Sampler m) = runReaderT m

sampleIOfixed :: SamplerIO a -> IO a
sampleIOfixed x = newIOGenM (mkStdGen 1729) >>= sampleWith x

geometric :: MonadDistribution m => m Int
geometric = do
  x <- random
  if x < 0.9
    then return 1
    else do y <- geometric
            return $ 1 + y

mhRunGeometric :: IO [Int]
mhRunGeometric = sampleIOfixed $ unweighted $ mh 100000 geometric
