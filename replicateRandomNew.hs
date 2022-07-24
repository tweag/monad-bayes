{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.Primitive (RealWorld)
import Control.Monad.Reader (replicateM)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask, runReaderT)
import System.Random.MWC
  ( Gen,
    GenIO,
    Variate (uniform, uniformR),
    createSystemRandom,
  )
import System.Random.MWC.Distributions qualified as MWC
import System.Random.Stateful (StatefulGen, uniformDouble01M, uniformRM)

class Monad m => MonadSample m where
  random :: m Double
  uniform :: Double -> Double -> m Double
  normal :: Double -> Double -> m Double

newtype Sampler g m a = Sampler (StatefulGen g m => ReaderT g m a)

runSampler :: StatefulGen g m => Sampler g m a -> ReaderT g m a
runSampler (Sampler s) = s

sampleWith :: (StatefulGen g m) => Sampler g m a -> g -> m a
sampleWith (Sampler m) = runReaderT m

instance Functor (Sampler g m) where
  fmap f (Sampler s) = Sampler $ fmap f s

instance Applicative (Sampler g m) where
  pure x = Sampler $ pure x
  (Sampler f) <*> (Sampler x) = Sampler $ f <*> x

instance Monad (Sampler g m) where
  (Sampler x) >>= f = Sampler $ x >>= runSampler . f

instance MonadSample (Sampler g m) where
  random = Sampler (ReaderT uniformDouble01M)

  uniform a b = Sampler (ReaderT $ uniformRM (a, b))
  normal m s = Sampler (ReaderT (MWC.normal m s))

newtype SamplerIO a = SamplerIO (ReaderT GenIO IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

sampleIOwith :: SamplerIO a -> Gen RealWorld -> IO a
sampleIOwith (SamplerIO m) = runReaderT m

instance MonadSample SamplerIO where
  random = SamplerIO $ ask >>= lift . System.Random.MWC.uniform

  uniform a b = SamplerIO $ ask >>= lift . uniformR (a, b)
  normal m s = SamplerIO $ ask >>= lift . MWC.normal m s

main :: IO ()
main = do
  g <- createSystemRandom
  xs <- sampleIOwith (replicateM 10000000 random) g
  print $ sum xs
