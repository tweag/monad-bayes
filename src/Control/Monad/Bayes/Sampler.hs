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
import Control.Monad.Trans (lift, MonadIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Numeric.LinearAlgebra ((<#), size)
import Data.Vector.Generic (replicateM)

import Numeric.LogDomain
import Control.Monad.Bayes.Class
import Statistics.Distribution.Polymorphic.Normal
import Statistics.Distribution.Polymorphic.Gamma
import Statistics.Distribution.Polymorphic.Beta
import Statistics.Distribution.Polymorphic.Uniform
import Statistics.Distribution.Polymorphic.Discrete
import Statistics.Distribution.Polymorphic.MVNormal
import Control.Monad.Bayes.Simple

-- | An `IO` based random sampler using the MWC-Random package.
newtype SamplerIO a = SamplerIO (ReaderT GenIO IO a)
  deriving(Functor, Applicative, Monad, MonadIO)

-- | Initialize PRNG using OS-supplied randomness.
-- For efficiency this operation should be applied at the very end, ideally once per program.
sampleIO :: SamplerIO a -> IO a
sampleIO (SamplerIO m) = createSystemRandom >>= runReaderT m

-- | Like `sampleIO`, but with a fixed random seed.
-- Useful for reproducibility.
sampleIOfixed :: SamplerIO a -> IO a
sampleIOfixed (SamplerIO m) = create >>= runReaderT m

instance HasCustomReal SamplerIO where
  type CustomReal SamplerIO = Double

-- | Helper for converting distributions supplied by MWC-Random
fromMWC :: (GenIO -> IO a) -> SamplerIO a
fromMWC s = SamplerIO $ ask >>= lift . s

instance Sampleable (Normal Double) SamplerIO where
  sample (Normal m s) = fromMWC $ MWC.normal m s

instance Sampleable (Gamma Double) SamplerIO where
  sample (Gamma a b) = fromMWC $ MWC.gamma a (recip b)

instance Sampleable (Beta Double) SamplerIO where
  sample (Beta a b) = fromMWC $ MWC.beta a b

instance Sampleable (Uniform Double) SamplerIO where
  sample (Uniform a b) = fromMWC $ uniformR (a,b)

instance Sampleable (Discrete Double Int) SamplerIO where
  sample (Discrete ps) = fromMWC $ MWC.categorical $ normalize ps

instance Sampleable MVNormal SamplerIO where
  sample (MVNormal m u) = do
    z <- replicateM (size m) $ fromMWC MWC.standard
    return $ m + (z <# u)

instance MonadDist SamplerIO where
  exponential rate = fromMWC $ MWC.exponential (recip rate)
  geometric p      = fromMWC $ MWC.geometric0 p
  bernoulli p      = fromMWC $ MWC.bernoulli p
  dirichlet ws     = fromMWC $ MWC.dirichlet ws
