{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


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
    Seed,
    SamplerST (SamplerST),
    runSamplerST,
    sampleST,
    sampleSTfixed,
  )
where

import Control.Monad.Bayes.Class
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.State (State, state)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Trans.Reader (ReaderT, ask, mapReaderT, runReaderT)
import System.Random.MWC
import qualified Data.Random as DR
import qualified Data.Random.Distribution.Beta as DR
import qualified Data.Random.Distribution.Bernoulli as DR
import qualified Data.Random.Distribution.Categorical as DR
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified System.Random.MWC.Distributions as MWC
import Control.Monad.Primitive
import qualified System.Random as R
import qualified System.Random.Stateful as RS
import qualified Control.Monad.Reader as MR




-- | An 'IO' based random sampler using the MWC-Random package.
newtype SamplerIO a = SamplerIO (ReaderT GenIO IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

newtype SamplerIO' a = SamplerIO' (ReaderT (RS.IOGenM RS.StdGen) IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

newtype SamplerIO'' a = SamplerIO'' (ReaderT (RS.STGenM RS.StdGen RealWorld) IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Initialize a pseudo-random number generator using randomness supplied by
-- the operating system.
-- For efficiency this operation should be applied at the very end, ideally
-- once per program.
sampleIO :: SamplerIO a -> IO a
sampleIO (SamplerIO m) = createSystemRandom >>= runReaderT m

-- | Like 'sampleIO', but with a fixed random seed.
-- Useful for reproducibility.
sampleIOfixed :: SamplerIO a -> IO a
sampleIOfixed (SamplerIO m) = create >>= runReaderT m

sampleIOfixed' :: SamplerIO' a -> IO a
sampleIOfixed' (SamplerIO' m) = (RS.newIOGenM (RS.mkStdGen 42)) >>= runReaderT m

-- | Like 'sampleIO' but with a custom pseudo-random number generator.
sampleIOwith :: SamplerIO a -> GenIO -> IO a
sampleIOwith (SamplerIO m) = runReaderT m

fromSamplerST :: SamplerST a -> SamplerIO a
fromSamplerST (SamplerST m) = SamplerIO $ mapReaderT stToIO m

fromSamplerST'' :: SamplerST' a -> SamplerIO'' a
fromSamplerST'' (SamplerST' m) = SamplerIO'' $ mapReaderT stToIO m

instance MonadSample SamplerIO where
  random = fromSamplerST random

-- instance MonadSample SamplerIO' where
--   random = fromSamplerST' random

-- | An 'ST' based random sampler using the @mwc-random@ package.
newtype SamplerST a = SamplerST (forall s. ReaderT (GenST s) (ST s) a)

newtype SamplerST' a = SamplerST' (forall s . ReaderT (RS.STGenM RS.StdGen s) (ST s) a)

runSamplerST :: SamplerST a -> ReaderT (GenST s) (ST s) a
runSamplerST (SamplerST s) = s

runSamplerST' :: SamplerST' a -> ReaderT (RS.STGenM RS.StdGen s) (ST s) a
runSamplerST' (SamplerST' s) = s

instance Functor SamplerST where
  fmap f (SamplerST s) = SamplerST $ fmap f s

instance Functor SamplerST' where
  fmap f (SamplerST' s) = SamplerST' $ fmap f s

instance Applicative SamplerST where
  pure x = SamplerST $ pure x
  (SamplerST f) <*> (SamplerST x) = SamplerST $ f <*> x

instance Applicative SamplerST' where
  pure x = SamplerST' $ pure x
  (SamplerST' f) <*> (SamplerST' x) = SamplerST' $ f <*> x

instance Monad SamplerST where
  (SamplerST x) >>= f = SamplerST $ x >>= runSamplerST . f

instance Monad SamplerST' where
  (SamplerST' x) >>= f = SamplerST' $ x >>= runSamplerST' . f

-- | Run the sampler with a supplied seed.
-- Note that 'State Seed' is much less efficient than 'SamplerST' for composing computation.
sampleST :: SamplerST a -> State Seed a
sampleST (SamplerST s) =
  state $ \seed -> runST $ do
    gen <- restore seed
    y <- runReaderT s gen
    finalSeed <- save gen
    return (y, finalSeed)

-- | Run the sampler with a fixed random seed.
sampleSTfixed :: SamplerST a -> a
sampleSTfixed (SamplerST s) = runST $ do
  gen <- create
  runReaderT s gen

sampleSTfixed' :: SamplerST' a -> a
sampleSTfixed' (SamplerST' s) = runST $ do
  gen <- RS.newSTGenM (RS.mkStdGen 42)
  runReaderT s gen

-- | Convert a distribution supplied by @mwc-random@.
fromMWC :: (forall s. GenST s -> ST s a) -> SamplerST a
fromMWC s = SamplerST $ ask >>= lift . s

fromMWC' :: (forall s . (RS.STGenM RS.StdGen s) -> ST s a) -> SamplerST' a
fromMWC' s = SamplerST' $ ask >>= lift . s

-- FIXME: standard deviation or variance?
-- FIXME: shape and scale in the right order?
-- FIXME: beta
instance MonadSample SamplerST' where
  random = fromMWC' $ RS.uniformDoublePositive01M

  uniform a b       = fromMWC' $ \g -> DR.sampleFrom g (DR.uniform a b)
  normal m s        = fromMWC' $ \g -> DR.sampleFrom g (DR.normal m s)
  gamma shape scale = fromMWC' $ \g -> DR.sampleFrom g (DR.gamma shape scale)
  beta a b          = fromMWC' $ \g -> DR.sampleFrom g (DR.beta a b)

  bernoulli p    = fromMWC' $ \g -> DR.sampleFrom g (DR.bernoulli p)
  categorical ps = fromMWC' $ \g -> DR.sampleFrom g (DR.categorical (zip (VG.toList ps) [0 ..]))
  geometric p    = error "Geometric not implemented in random-fu :("

instance MonadSample SamplerST where
  random = fromMWC System.Random.MWC.uniform

  uniform a b = fromMWC $ uniformR (a, b)
  normal m s = fromMWC $ MWC.normal m s
  gamma shape scale = fromMWC $ MWC.gamma shape scale
  beta a b = fromMWC $ MWC.beta a b

  bernoulli p = fromMWC $ MWC.bernoulli p
  categorical ps = fromMWC $ MWC.categorical ps
  geometric p = fromMWC $ MWC.geometric0 p
