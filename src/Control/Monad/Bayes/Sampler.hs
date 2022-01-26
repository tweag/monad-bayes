{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

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
  ( SamplerIO,
    sampleIO,
    sampleIOStdGen,
    sampleIOfixed,
    sampleIOwith,
    -- Seed,
    SamplerST (SamplerST),
    runSamplerST,
    -- sampleST,
    sampleSTfixed,
  )
where

import Control.Monad.Bayes.Class
import Control.Monad.Primitive
import qualified Control.Monad.Reader as MR
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Trans.Reader (ReaderT, ask, mapReaderT, runReaderT)
import qualified Data.Random as DR
import qualified Data.Random.Distribution.Bernoulli as DR
import qualified Data.Random.Distribution.Beta as DR
import qualified Data.Random.Distribution.Categorical as DR
import qualified Data.Vector.Generic as VG
import qualified System.Random.Stateful as RS

-- | An 'IO' based random sampler using the @random-fu@ package.
newtype SamplerIO g a = SamplerIO (ReaderT (RS.STGenM g RealWorld) IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

sampleIO :: SamplerIO g a -> g -> IO a
sampleIO m g = stToIO (RS.newSTGenM g) >>= sampleIOwith m

-- | Initialize a pseudo-random number generator using randomness supplied by
-- the operating system.
-- For efficiency this operation should be applied at the very end, ideally
-- once per program.

-- FIXME: A lie
sampleIOStdGen :: SamplerIO RS.StdGen a -> IO a
sampleIOStdGen m = sampleIO m (RS.mkStdGen 42)

-- | Like 'sampleIO', but with a fixed random seed.
-- Useful for reproducibility.
sampleIOfixed :: SamplerIO RS.StdGen a -> IO a
sampleIOfixed m = sampleIO m (RS.mkStdGen 42)

-- | Like 'sampleIO' but with a custom pseudo-random number generator.
sampleIOwith :: SamplerIO g a -> RS.STGenM g RealWorld -> IO a
sampleIOwith (SamplerIO m) = runReaderT m

fromSamplerST :: RS.RandomGen g => SamplerST a -> SamplerIO g a
fromSamplerST (SamplerST m) = SamplerIO $ mapReaderT stToIO m

instance RS.RandomGen g => MonadSample (SamplerIO g) where
  random = fromSamplerST random

-- | An 'ST' based random sampler using the @random-fu@ package.
newtype SamplerST a = SamplerST (forall s g. RS.RandomGen g => ReaderT (RS.STGenM g s) (ST s) a)

runSamplerST :: RS.RandomGen g => SamplerST a -> ReaderT (RS.STGenM g s) (ST s) a
runSamplerST (SamplerST s) = s

instance Functor SamplerST where
  fmap f (SamplerST s) = SamplerST $ fmap f s

instance Applicative SamplerST where
  pure x = SamplerST $ pure x
  (SamplerST f) <*> (SamplerST x) = SamplerST $ f <*> x

instance Monad SamplerST where
  (SamplerST x) >>= f = SamplerST $ x >>= runSamplerST . f

-- | Run the sampler with a fixed random seed.
sampleSTfixed :: SamplerST a -> a
sampleSTfixed (SamplerST s) = runST $ do
  gen <- RS.newSTGenM (RS.mkStdGen 42)
  runReaderT s gen

-- | Convert a distribution supplied by @random-fu@.
fromRandomFu :: (forall s g. RS.RandomGen g => RS.STGenM g s -> ST s a) -> SamplerST a
fromRandomFu s = SamplerST $ ask >>= lift . s

-- FIXME: standard deviation or variance?
-- FIXME: shape and scale in the right order?
-- FIXME: beta
instance MonadSample SamplerST where
  random = fromRandomFu RS.uniformDoublePositive01M

  uniform a b = fromRandomFu $ \g -> DR.sampleFrom g (DR.uniform a b)
  normal m s = fromRandomFu $ \g -> DR.sampleFrom g (DR.normal m s)
  gamma shape scale = fromRandomFu $ \g -> DR.sampleFrom g (DR.gamma shape scale)
  beta a b = fromRandomFu $ \g -> DR.sampleFrom g (DR.beta a b)

  bernoulli p = fromRandomFu $ \g -> DR.sampleFrom g (DR.bernoulli p)
  categorical ps = fromRandomFu $ \g -> DR.sampleFrom g (DR.categorical (zip (VG.toList ps) [0 ..]))
  geometric _p = error "Geometric not implemented in random-fu :("
