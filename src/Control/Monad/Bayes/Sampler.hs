{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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
    sampleIOfixed,
    sampleIOwith,
    sampleWith,
    Seed,
    SamplerST (SamplerST),
    runSamplerST,
    -- sampleST,
    -- sampleSTfixed,
    sampleSTwith,
  )
where

import Control.Monad.Bayes.Class
import Control.Monad.Primitive
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.State (State, state)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Trans.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Data.Void
import System.Random.MWC
import qualified System.Random.MWC.Distributions as MWC
import System.Random.Stateful (IOGenM, STGenM, StdGen, mkStdGen, newIOGenM, newSTGenM)
import qualified System.Random.Stateful as SR

-- | An 'IO' based random sampler using the MWC-Random package.
newtype SamplerIO g a = SamplerIO (SR.StatefulGen g IO => ReaderT g IO a)

newtype Sampler g m a = Sampler (SR.StatefulGen g m => ReaderT g m a)

instance Functor (SamplerIO g) where
  fmap f (SamplerIO s) = SamplerIO $ fmap f s

instance Applicative (SamplerIO g) where
  pure x = SamplerIO $ pure x
  (SamplerIO f) <*> (SamplerIO x) = SamplerIO $ f <*> x

runSamplerIO :: SR.StatefulGen g IO => SamplerIO g a -> ReaderT g IO a
runSamplerIO (SamplerIO s) = s

instance Monad (SamplerIO g) where
  (SamplerIO x) >>= f = SamplerIO $ x >>= runSamplerIO . f

instance Functor (Sampler g m) where
  fmap f (Sampler s) = Sampler $ fmap f s

instance Applicative (Sampler g m) where
  pure x = Sampler $ pure x
  (Sampler f) <*> (Sampler x) = Sampler $ f <*> x

runSampler :: SR.StatefulGen g m => Sampler g m a -> ReaderT g m a
runSampler (Sampler s) = s

instance Monad (Sampler g m) where
  (Sampler x) >>= f = Sampler $ x >>= runSampler . f

-- | Initialize a pseudo-random number generator using randomness supplied by
-- the operating system.
-- For efficiency this operation should be applied at the very end, ideally
-- once per program.
sampleIO :: SamplerIO (IOGenM StdGen) b -> IO b
sampleIO x = newIOGenM (mkStdGen 1729) >>= sampleIOwith x

-- | Like 'sampleIO', but with a fixed random seed.
-- Useful for reproducibility.
sampleIOfixed :: SamplerIO (IOGenM StdGen) a -> IO a
sampleIOfixed x = newIOGenM (mkStdGen 1729) >>= sampleIOwith x

-- | Like 'sampleIO' but with a custom pseudo-random number generator.
sampleIOwith :: SR.StatefulGen r IO => SamplerIO r a -> r -> IO a
sampleIOwith (SamplerIO m) = runReaderT m

sampleWith :: (SR.StatefulGen r m) => Sampler r m a -> r -> m a
sampleWith (Sampler m) = runReaderT m

instance MonadSample (SamplerIO g) where
  random = SamplerIO ((\s -> ask >>= lift . s) (uniformRM (0, 1))) -- FIXME: There is a special function for this

instance MonadSample (Sampler g m) where
  random = Sampler ((\s -> ask >>= lift . s) (uniformRM (0, 1))) -- FIXME: There is a special function for this
  normal m s = Sampler (fromMWC' (MWC.normal m s))

-- | An 'ST' based random sampler using the @mwc-random@ package.
newtype SamplerST g a = SamplerST (forall s. SR.StatefulGen g (ST s) => ReaderT g (ST s) a)

runSamplerST :: SR.StatefulGen g (ST s) => SamplerST g a -> ReaderT g (ST s) a
runSamplerST (SamplerST s) = s

instance Functor (SamplerST g) where
  fmap f (SamplerST s) = SamplerST $ fmap f s

instance Applicative (SamplerST g) where
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
sampleSTfixed :: SamplerST (STGenM StdGen s) a -> ST s a
sampleSTfixed (SamplerST s) = do
  gen <- newSTGenM (mkStdGen 1729)
  runReaderT s gen

-- | Like 'sampleST' but with a custom pseudo-random number generator.
sampleSTwith :: SR.StatefulGen r (ST s) => SamplerST r a -> r -> ST s a
sampleSTwith (SamplerST s) = runReaderT s

fromMWC :: (g -> (ST s) a) -> ReaderT g (ST s) a
fromMWC = (\s -> ask >>= lift . s)

fromMWC' :: Monad m => (a -> m b) -> ReaderT a m b
fromMWC' = (\s -> ask >>= lift . s)

instance MonadSample (SamplerST g) where
  random = SamplerST (fromMWC (uniformRM (0, 1))) -- FIXME: There is a special function for this

  uniform a b = SamplerST (fromMWC $ uniformRM (a, b))
  normal m s = SamplerST (fromMWC (MWC.normal m s))
  gamma shape scale = SamplerST (fromMWC $ MWC.gamma shape scale)
  beta a b = SamplerST (fromMWC $ MWC.beta a b)

  bernoulli p = SamplerST (fromMWC $ MWC.bernoulli p)
  categorical ps = SamplerST (fromMWC $ MWC.categorical ps)
  geometric p = SamplerST (fromMWC $ MWC.geometric0 p)
