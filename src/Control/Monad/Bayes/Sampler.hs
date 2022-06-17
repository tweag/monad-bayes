{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    -- sampleIO,
    -- sampleIOfixed,
    sampleIOwith,
    Seed,
    SamplerST (SamplerST),
    runSamplerST,
    -- sampleST,
    -- sampleSTfixed,
  )
where

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
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.State (State, state)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Trans.Reader (ReaderT, ask, mapReaderT, runReaderT)
import System.Random.MWC
  ( GenIO,
    GenST,
    Seed,
    Variate (uniform, uniformR),
    create,
    createSystemRandom,
    restore,
    save,
  )
import System.Random.MWC.Distributions qualified as MWC

import Data.Void
import qualified System.Random.Stateful as SR

-- | An 'IO' based random sampler using the MWC-Random package.
newtype SamplerIO g a = SamplerIO (SR.StatefulGen g IO => ReaderT g IO a)

instance Functor (SamplerIO g) where
  fmap f (SamplerIO s) = SamplerIO $ fmap f s

instance Applicative (SamplerIO g) where
  pure x = SamplerIO $ pure x
  (SamplerIO f) <*> (SamplerIO x) = SamplerIO $ f <*> x

runSamplerIO :: SR.StatefulGen g IO => SamplerIO g a -> ReaderT g IO a
runSamplerIO (SamplerIO s) = s

instance Monad (SamplerIO g) where
  (SamplerIO x) >>= f = SamplerIO $ x >>= runSamplerIO . f

-- | Initialize a pseudo-random number generator using randomness supplied by
-- the operating system.
-- For efficiency this operation should be applied at the very end, ideally
-- once per program.
sampleIO :: SamplerIO g a -> IO a
sampleIO (SamplerIO m) = undefined -- createSystemRandom >>= runReaderT m

-- | Like 'sampleIO', but with a fixed random seed.
-- Useful for reproducibility.
sampleIOfixed :: SamplerIO g a -> IO a
sampleIOfixed (SamplerIO m) = undefined -- create >>= runReaderT m

-- | Like 'sampleIO' but with a custom pseudo-random number generator.
sampleIOwith :: SR.StatefulGen r IO => SamplerIO r a -> r -> IO a
sampleIOwith (SamplerIO m) = runReaderT m

instance MonadSample (SamplerIO g) where
  random = SamplerIO ((\s -> ask >>= lift . s) (uniformRM (0, 1))) -- FIXME: There is a special function for this

-- | An 'ST' based random sampler using the @mwc-random@ package.
newtype SamplerST g a = SamplerST (forall s. SR.StatefulGen g (ST s) => ReaderT g (ST s) a)

runSamplerST :: SR.StatefulGen g (ST s) => SamplerST g a -> ReaderT g (ST s) a
runSamplerST (SamplerST s) = s

instance Functor (SamplerST g) where
  fmap f (SamplerST s) = SamplerST $ fmap f s

instance Applicative (SamplerST g)where
  pure x = SamplerST $ pure x
  (SamplerST f) <*> (SamplerST x) = SamplerST $ f <*> x

instance Monad (SamplerST g) where
  (SamplerST x) >>= f = SamplerST $ x >>= runSamplerST . f

-- | Run the sampler with a supplied seed.
-- Note that 'State Seed' is much less efficient than 'SamplerST' for composing computation.
sampleST :: SamplerST g a -> State Seed a
sampleST (SamplerST s) =
  undefined
  -- state $ \seed -> runST $ do
  --   gen <- restore seed
  --   y <- runReaderT s gen
  --   finalSeed <- save gen
  --   return (y, finalSeed)

-- | Run the sampler with a fixed random seed.
sampleSTfixed :: SamplerST g a -> a
sampleSTfixed (SamplerST s) = runST $ do
  undefined
  -- gen <- create
  -- runReaderT s gen

-- | Convert a distribution supplied by @mwc-random@.
fromMWC :: (g -> (ST s) a) -> ReaderT g (ST s) a
fromMWC = (\s -> ask >>= lift . s)

instance MonadSample (SamplerST g) where
  random = SamplerST (fromMWC (uniformRM (0, 1))) -- FIXME: There is a special function for this

  uniform a b       = SamplerST (fromMWC $ uniformRM (a, b))
  normal m s        = SamplerST (fromMWC (MWC.normal m s))
  gamma shape scale = SamplerST (fromMWC $ MWC.gamma shape scale)
  beta a b          = SamplerST (fromMWC $ MWC.beta a b)

  bernoulli p    = SamplerST (fromMWC $ MWC.bernoulli p)
  categorical ps = SamplerST (fromMWC $ MWC.categorical ps)
  geometric p    = SamplerST (fromMWC $ MWC.geometric0 p)
