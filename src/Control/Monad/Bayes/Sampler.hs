{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Control.Monad.Bayes.Sampler
-- Description : Pseudo-random sampling monads
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- 'SamplerIO' and 'SamplerST' are instances of 'MonadSample'. Apply a 'MonadCond'
-- transformer to obtain a 'MonadInfer' that can execute probabilistic models.
module Control.Monad.Bayes.Sampler
  ( sampleIOwith,
    sampleWith,
  )
where

import Control.Foldl qualified as F hiding (random)
import Control.Monad.Bayes.Class
  ( MonadSample
      ( bernoulli,
        beta,
        categorical,
        gamma,
        geometric,
        normal,
        random,
        uniform
      ),
  )
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask, runReaderT)
import System.Random.MWC
  ( Gen,
    GenIO,
    Variate (uniform, uniformR),
  )
import System.Random.MWC.Distributions qualified as MWC
import System.Random.Stateful (StatefulGen, uniformDouble01M, uniformRM)

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
  gamma shape scale = Sampler (ReaderT $ MWC.gamma shape scale)
  beta a b = Sampler (ReaderT $ MWC.beta a b)

  bernoulli p = Sampler (ReaderT $ MWC.bernoulli p)
  categorical ps = Sampler (ReaderT $ MWC.categorical ps)
  geometric p = Sampler (ReaderT $ MWC.geometric0 p)

-- | An 'IO' based random sampler using the MWC-Random package.
newtype SamplerIO a = SamplerIO (ReaderT GenIO IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Like 'sampleIO' but with a custom pseudo-random number generator.
sampleIOwith :: SamplerIO a -> Gen F.RealWorld -> IO a
sampleIOwith (SamplerIO m) = runReaderT m

instance MonadSample SamplerIO where
  random = SamplerIO $ ask >>= lift . System.Random.MWC.uniform

  uniform a b = SamplerIO $ ask >>= lift . uniformR (a, b)
  normal m s = SamplerIO $ ask >>= lift . MWC.normal m s
  gamma shape scale = SamplerIO $ ask >>= lift . MWC.gamma shape scale
  beta a b = SamplerIO $ ask >>= lift . MWC.beta a b

  bernoulli p = SamplerIO $ ask >>= lift . MWC.bernoulli p
  categorical ps = SamplerIO $ ask >>= lift . MWC.categorical ps
  geometric p = SamplerIO $ ask >>= lift . MWC.geometric0 p
