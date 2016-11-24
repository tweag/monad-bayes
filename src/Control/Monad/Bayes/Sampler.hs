{-|
Module      : Control.Monad.Bayes.Sampler
Description : Psuedo-random sampling monads
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Sampler (
    SamplerIO,
    sampleIO,
    sampleIOfixed
               ) where

import System.Random.MWC (GenIO, create, createSystemRandom, uniformR)
import qualified System.Random.MWC.Distributions as MWC
import Data.Vector (fromList)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)

import Control.Monad.Bayes.Class

-- | An `IO` based random sampler using the MWC-Random package.
newtype SamplerIO a = SamplerIO (ReaderT GenIO IO a)
  deriving(Functor, Applicative, Monad)

-- | Initialize PRNG using OS-supplied randomness.
-- For efficiency this operation should be applied at the very end, ideally once per program.
sampleIO :: SamplerIO a -> IO a
sampleIO (SamplerIO m) = createSystemRandom >>= runReaderT m

-- | Like `sampleIO`, but with a fixed random seed.
-- Useful for reproducibility.
sampleIOfixed :: SamplerIO a -> IO a
sampleIOfixed (SamplerIO m) = create >>= runReaderT m

type instance CustomReal SamplerIO = Double

-- | Helper for converting distributions supplied by MWC-Random
fromMWC :: (GenIO -> IO a) -> SamplerIO a
fromMWC s = SamplerIO $ ask >>= lift . s

instance MonadDist SamplerIO where
  discrete ps      = fromMWC $ MWC.categorical (Data.Vector.fromList ps)
  normal m s       = fromMWC $ MWC.normal m s
  gamma a b        = fromMWC $ MWC.gamma a (recip b)
  beta a b         = fromMWC $ MWC.beta a b
  uniform a b      = fromMWC $ uniformR (a,b)
  exponential rate = fromMWC $ MWC.exponential (recip rate)
  geometric p      = fromMWC $ MWC.geometric0 p
  bernoulli p      = fromMWC $ MWC.bernoulli p
  dirichlet ws     = fromMWC $ MWC.dirichlet ws
