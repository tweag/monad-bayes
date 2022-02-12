{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
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
newtype SamplerIO g a = SamplerIO (ReaderT g IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

sampleIO :: SamplerIO g a -> g -> IO a
sampleIO m = sampleIOwith m

-- | Initialize a pseudo-random number generator using randomness supplied by
-- the operating system.
-- For efficiency this operation should be applied at the very end, ideally
-- once per program.

-- FIXME: A lie
sampleIOStdGen :: SamplerIO (RS.STGenM RS.StdGen RealWorld) a -> IO a
sampleIOStdGen m = sampleIO m =<< stToIO (RS.newSTGenM (RS.mkStdGen 42))

-- | Like 'sampleIO', but with a fixed random seed.
-- Useful for reproducibility.
sampleIOfixed :: SamplerIO (RS.STGenM RS.StdGen RealWorld) a -> IO a
sampleIOfixed m = sampleIO m =<< stToIO (RS.newSTGenM (RS.mkStdGen 42))

-- | Like 'sampleIO' but with a custom pseudo-random number generator.
sampleIOwith :: SamplerIO g a -> g -> IO a
sampleIOwith (SamplerIO m) = runReaderT m

fromSamplerST :: RS.StatefulGen g (ST RealWorld) => SamplerST a -> SamplerIO g a
fromSamplerST (SamplerST m) = SamplerIO $ mapReaderT stToIO m

instance RS.StatefulGen g (ST RealWorld) => MonadSample (SamplerIO g) where
  random = fromSamplerST random

-- | An 'ST' based random sampler using the @random-fu@ package.
newtype SamplerST a = SamplerST (forall s g. RS.StatefulGen g (ST s) => ReaderT g (ST s) a)

runSamplerST :: RS.StatefulGen g (ST s) => SamplerST a -> ReaderT g (ST s) a
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
fromRandomFu :: (forall s g. RS.StatefulGen g (ST s) => g -> ST s a) -> SamplerST a
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
