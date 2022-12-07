{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Monad.State (MonadState (get, put), StateT, evalStateT, runStateT)
import Control.Monad.Writer
import Control.Applicative (liftA2)
import Control.Monad.Reader (MonadIO, ReaderT (..))

import Statistics.Distribution.DiscreteUniform (discreteUniformAB)
import Statistics.Distribution.Uniform (uniformDistr)
import Statistics.Distribution (probability, DiscreteDistr, ContDistr, quantile)

import Numeric.Log
import System.Random.Stateful (IOGenM (..), STGenM, StatefulGen, StdGen, initStdGen, mkStdGen, newIOGenM, newSTGenM, uniformDouble01M, uniformRM)
import System.Random.MWC.Distributions qualified as MWC

import Debug.Trace qualified as D


data Trace a = Trace { variables :: [Double], output :: a, probDensity :: Log Double }

instance Show a => Show (Trace a) where
  show (Trace vs o pdf) = "Trace " ++ show vs ++ " " ++ show o ++ " " ++ show pdf

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

class Monad m => MonadDistribution m where
  random :: m Double
  uniform :: Double -> Double -> m Double
  uniform a b = draw (uniformDistr a b)
  bernoulli :: Double -> m Bool
  bernoulli p = fmap (< p) random

draw :: (ContDistr d, MonadDistribution m) => d -> m Double
draw d =  do x <- random
             let y = quantile d x
             return y

instance MonadDistribution m => MonadDistribution (StateT s m) where
  random = lift random
  bernoulli = lift . bernoulli

fromPMF :: MonadDistribution m => (Int -> Double) -> m Int
fromPMF p = f 0 1
  where
    f i r = do
      when (r < 0) $ error "fromPMF: total PMF above 1"
      let q = p i
      when (q < 0 || q > 1) $ error "fromPMF: invalid probability value"
      b <- bernoulli (q / r)
      if b then pure i else f (i + 1) (r - q)

discrete :: (DiscreteDistr d, MonadDistribution m) => d -> m Int
discrete = fromPMF . probability

newtype Density m a = Density {runDensity :: WriterT [Double] (StateT [Double] m) a} deriving newtype (Functor, Applicative, Monad)

instance MonadTrans Density where
  lift = Density . lift . lift

instance Monad m => MonadState [Double] (Density m) where
  get = Density $ lift $ get
  put = Density . lift . put

instance Monad m => MonadWriter [Double] (Density m) where
  tell = Density . tell
  listen = Density . listen . runDensity
  pass = Density . pass . runDensity

instance MonadDistribution m => MonadDistribution (Density m) where
  random = do
    trace <- get
    x <- case trace of
      [] -> random
      r : xs -> put xs >> pure r
    tell [x]
    pure x

density :: Monad m => Density m b -> [Double] -> m (b, [Double])
density (Density m) = evalStateT (runWriterT m)

newtype Weighted m a = Weighted (StateT (Log Double) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadDistribution)

weighted :: Weighted m a -> m (a, Log Double)
weighted (Weighted m) = runStateT m 1

unweighted :: Functor m => Weighted m a -> m a
unweighted = fmap fst . weighted

mhTrans :: MonadDistribution m => (Weighted (Density m)) a -> Trace a -> m (Trace a)
mhTrans m t@Trace {variables = us, probDensity = p} = do
  let n = length us
  D.trace (show n) $ return ()
  us' <- do
    i <- discrete $ discreteUniformAB 0 (n - 1)
    D.trace (show i) $ return ()
    u' <- random
    D.trace (show u') $ return ()
    case splitAt i us of
      (xs, _ : ys) -> return $ xs ++ (u' : ys)
      _ -> error "impossible"
  D.trace (show us') $ return ()
  ((b, q), vs) <- density (weighted m) us'
  D.trace (show vs) $ return ()
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  return $ if accept then Trace vs b q else t

data Traced m a = Traced { model :: Weighted (Density m) a, traceDist :: m (Trace a) }

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

instance MonadTrans Traced where
  lift m = Traced (lift $ lift m) (fmap pure m)

instance MonadDistribution m => MonadDistribution (Traced m) where
  random = Traced random (fmap singleton random)

singleton :: Double -> Trace Double
singleton u = Trace {variables = [u], output = u, probDensity = 1}

bind :: Monad m => m (Trace a) -> (a -> m (Trace b)) -> m (Trace b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  return $ t2 {variables = variables t1 ++ variables t2, probDensity = probDensity t1 * probDensity t2}

mh :: forall m a . Show a => MonadDistribution m => Int -> Traced m a -> m [a]
mh n (Traced m d) = (fmap (map output)) (f n)
  where
    f :: Int -> m [Trace a]
    f k | k <= 0    = fmap pure d
        | otherwise = do xs <- f (k - 1)
                         D.trace (show $ length xs) $ return ()
                         D.trace (show $ head xs) $ return ()
                         y <- mhTrans m (head xs)
                         return (y : xs)

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
  if x < 0.2
    then return 1
    else do y <- geometric
            return $ 1 + y

mhRunGeometric :: IO [Int]
mhRunGeometric = sampleIOfixed $ unweighted $ mh 1 geometric
