{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
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
  -- ( SamplerIO,
  --   sampleIO,
  --   sampleIOStdGen,
  --   sampleIOfixed,
  --   sampleIOwith,
  --   -- Seed,
  --   SamplerST (SamplerST),
  --   runSamplerST,
  --   sampleSTfixed,
  -- )
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
import GHC.Word
import qualified System.Random.Stateful as RS

import qualified System.Random.MWC as MWC

newtype Sampler g a = Sampler { unSampler :: forall m. RS.StatefulGen g m => ReaderT g m a }

instance Functor (Sampler g) where
  fmap f (Sampler x) = Sampler (fmap f x)

instance Applicative (Sampler g) where
  pure x = Sampler $ pure x
  (Sampler f) <*> (Sampler x) = Sampler (f <*> x)

instance Monad (Sampler g) where
  return = pure
  (Sampler x) >>= f = Sampler (x >>= \y -> unSampler $ f y)


sampler :: (forall g m. RS.StatefulGen g m => g -> m a) -> Sampler g a
sampler f = Sampler (ask >>= lift . f)

instance (RS.StatefulGen g (ST RealWorld)) => MonadSample (Sampler g) where
  random = sampler RS.uniformDoublePositive01M

  uniform a b = sampler $ \g -> DR.sampleFrom g (DR.uniform a b)
  normal m s = sampler $ \g -> DR.sampleFrom g (DR.normal m s)
  gamma shape scale = sampler $ \g -> DR.sampleFrom g (DR.gamma shape scale)
  beta a b = sampler $ \g -> DR.sampleFrom g (DR.beta a b)

  bernoulli p = sampler $ \g -> DR.sampleFrom g (DR.bernoulli p)
  categorical ps = sampler $ \g -> DR.sampleFrom g (DR.categorical (zip (VG.toList ps) [0 ..]))
  geometric _p = error "Geometric not implemented in random-fu :("

runSampler :: RS.StatefulGen g m => Sampler g a -> g -> m a
runSampler (Sampler m) = runReaderT m

runSamplerIO :: RS.StatefulGen g IO => Sampler g a -> g -> IO a
runSamplerIO = runSampler

runSamplerST :: RS.StatefulGen g (ST s) => Sampler g a -> g -> ST s a
runSamplerST = runSampler

foo1 :: PrimMonad m => Sampler (MWC.Gen (PrimState m)) b -> m b
foo1 m = do
  g <- MWC.create
  runSampler m g

bar1 :: Sampler (RS.STGenM RS.StdGen s) b -> ST s b
bar1 m = do
  g <- RS.newSTGenM (RS.mkStdGen 42)
  runSampler m g

newtype SamplerIO g a = SamplerIO (RS.StatefulGen g IO => Sampler g a)

-- | Initialize a pseudo-random number generator using randomness supplied by
-- the operating system.
-- For efficiency this operation should be applied at the very end, ideally
-- once per program.

-- FIXME: A lie
-- sampleIOStdGen :: SamplerIO (RS.STGenM RS.StdGen RealWorld) a -> IO a
-- sampleIOStdGen (SamplerIO m) = stToIO $ runSampler m =<< RS.newSTGenM (RS.mkStdGen 42)


newtype SamplerST g a = SamplerST (forall s. RS.StatefulGen g (ST s) => Sampler g a)

-- -- | Like 'sampleIO', but with a fixed random seed.
-- -- Useful for reproducibility.
-- sampleIOFixed :: Sampler (RS.STGenM RS.StdGen RealWorld) a -> IO a
-- sampleIOFixed m = stToIO $ sample m =<< RS.newSTGenM (RS.mkStdGen 42)

-- -- fromSamplerST :: (RS.StatefulGen g (ST RealWorld), RS.StatefulGen g' IO) => Sampler g a -> Sampler g' a
-- -- fromSamplerST (Sampler m) = Sampler $ mapReaderT stToIO m

-- -- | An 'ST' based random sampler using the @random-fu@ package.
-- newtype SamplerST a = SamplerST (forall s g. RS.StatefulGen g (ST s) => ReaderT g (ST s) a)

-- runSamplerST :: RS.StatefulGen g (ST s) => SamplerST a -> ReaderT g (ST s) a
-- runSamplerST (SamplerST s) = s

-- instance Functor SamplerST where
--   fmap f (SamplerST s) = SamplerST $ fmap f s

-- instance Applicative SamplerST where
--   pure x = SamplerST $ pure x
--   (SamplerST f) <*> (SamplerST x) = SamplerST $ f <*> x

-- instance Monad SamplerST where
--   (SamplerST x) >>= f = SamplerST $ x >>= runSamplerST . f

-- -- | Run the sampler with a fixed random seed.
-- sampleSTfixed :: SamplerST a -> a
-- sampleSTfixed (SamplerST s) = runST $ do
--   gen <- RS.newSTGenM (RS.mkStdGen 42)
--   runReaderT s gen

-- -- | Convert a distribution supplied by @random-fu@.
-- fromRandomFu :: (forall s g. RS.StatefulGen g (ST s) => g -> ST s a) -> SamplerST a
-- fromRandomFu s = SamplerST $ ask >>= lift . s

-- -- FIXME: standard deviation or variance?
-- -- FIXME: shape and scale in the right order?
-- -- FIXME: beta
-- instance MonadSample SamplerST where
--   random = fromRandomFu RS.uniformDoublePositive01M

--   uniform a b = fromRandomFu $ \g -> DR.sampleFrom g (DR.uniform a b)
--   normal m s = fromRandomFu $ \g -> DR.sampleFrom g (DR.normal m s)
--   gamma shape scale = fromRandomFu $ \g -> DR.sampleFrom g (DR.gamma shape scale)
--   beta a b = fromRandomFu $ \g -> DR.sampleFrom g (DR.beta a b)

--   bernoulli p = fromRandomFu $ \g -> DR.sampleFrom g (DR.bernoulli p)
--   categorical ps = fromRandomFu $ \g -> DR.sampleFrom g (DR.categorical (zip (VG.toList ps) [0 ..]))
--   geometric _p = error "Geometric not implemented in random-fu :("
