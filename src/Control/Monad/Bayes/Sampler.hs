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
  ( SamplerIO,
    sampleIO,
    sampleIOfixed,
    sampleIOwith,
    Seed,
    SamplerST (SamplerST),
    runSamplerST,
    sampleST,
    sampleSTfixed,
    toBins,
    sampleMean,
    sampler
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
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.State (State, state)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Trans.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Data.Fixed (mod')
import Numeric.Log (Log (ln))
import System.Random.MWC
  ( Gen,
    GenIO,
    GenST,
    Seed,
    Variate (uniform, uniformR),
    create,
    createSystemRandom,
    restore,
    save,
  )
import System.Random.MWC.Distributions qualified as MWC

-- | An 'IO' based random sampler using the MWC-Random package.
newtype SamplerIO a = SamplerIO (ReaderT GenIO IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Initialize a pseudo-random number generator using randomness supplied by
-- the operating system.
-- For efficiency this operation should be applied at the very end, ideally
-- once per program.
sampler :: SamplerIO a -> IO a
sampler = sampleIO

sampleIO :: SamplerIO a -> IO a
sampleIO (SamplerIO m) = createSystemRandom >>= runReaderT m

-- Useful for reproducibility.
sampleIOfixed :: SamplerIO a -> IO a
sampleIOfixed (SamplerIO m) = create >>= runReaderT m

-- | Like 'sampleIO' but with a custom pseudo-random number generator.
sampleIOwith :: SamplerIO a -> Gen F.RealWorld -> IO a
sampleIOwith (SamplerIO m) = runReaderT m

fromSamplerST :: SamplerST a -> SamplerIO a
fromSamplerST (SamplerST m) = SamplerIO $ mapReaderT stToIO m

instance MonadSample SamplerIO where
  random = fromSamplerST random

-- | An 'ST' based random sampler using the @mwc-random@ package.
newtype SamplerST a = SamplerST (forall s. ReaderT (GenST s) (ST s) a)

runSamplerST :: SamplerST a -> ReaderT (GenST s) (ST s) a
runSamplerST (SamplerST s) = s

instance Functor SamplerST where
  fmap f (SamplerST s) = SamplerST $ fmap f s

instance Applicative SamplerST where
  pure x = SamplerST $ pure x
  (SamplerST f) <*> (SamplerST x) = SamplerST $ f <*> x

instance Monad SamplerST where
  (SamplerST x) >>= f = SamplerST $ x >>= runSamplerST . f

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

-- | Convert a distribution supplied by @mwc-random@.
fromMWC :: (forall s. GenST s -> ST s a) -> SamplerST a
fromMWC s = SamplerST $ ask >>= lift . s

instance MonadSample SamplerST where
  random = fromMWC System.Random.MWC.uniform

  uniform a b = fromMWC $ uniformR (a, b)
  normal m s = fromMWC $ MWC.normal m s
  gamma shape scale = fromMWC $ MWC.gamma shape scale
  beta a b = fromMWC $ MWC.beta a b

  bernoulli p = fromMWC $ MWC.bernoulli p
  categorical ps = fromMWC $ MWC.categorical ps
  geometric p = fromMWC $ MWC.geometric0 p

type Bin = (Double, Double)

-- | binning function. Useful when you want to return the bin that
-- a random variable falls into, so that you can show a histogram of samples
toBin ::
  -- | bin size
  Double ->
  -- | number
  Double ->
  Bin
toBin binSize n = let lb = n `mod'` binSize in (n - lb, n - lb + binSize)

toBins :: Double -> [Double] -> [Double]
toBins binWidth = fmap (fst . toBin binWidth)

sampleMean :: [(Double, Log Double)] -> Double
sampleMean samples =
  let z = F.premap (ln . exp . snd) F.sum
      w = (F.premap (\(x, y) -> x * ln (exp y)) F.sum)
      s = (/) <$> w <*> z
   in F.fold s samples
