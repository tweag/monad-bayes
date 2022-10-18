{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module      : Control.Monad.Bayes.Sampler
-- Description : Pseudo-random sampling monads
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- 'SamplerIO' and 'SamplerST' are instances of 'MonadDistribution'. Apply a 'MonadFactor'
-- transformer to obtain a 'MonadMeasure' that can execute probabilistic models.
module Control.Monad.Bayes.Sampler.Strict
  ( Sampler,
    SamplerIO,
    SamplerST,
    sampleIO,
    sampleIOfixed,
    sampleWith,
    sampleSTfixed,
    sampleMean,
    sampler,
  )
where

import Control.Foldl qualified as F hiding (random)
import Control.Monad.Bayes.Class
  ( MonadDistribution
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
import Control.Monad.Reader (MonadIO, ReaderT (..))
import Control.Monad.ST (ST)
import Numeric.Log (Log (ln))
import System.Random.MWC.Distributions qualified as MWC
import System.Random.Stateful (IOGenM (..), STGenM, StatefulGen, StdGen, initStdGen, mkStdGen, newIOGenM, newSTGenM, uniformDouble01M, uniformRM)

-- | The sampling interpretation of a probabilistic program
-- Here m is typically IO or ST
newtype Sampler g m a = Sampler (ReaderT g m a) deriving (Functor, Applicative, Monad, MonadIO)

-- | convenient type synonym to show specializations of Sampler
-- to particular pairs of monad and RNG
type SamplerIO = Sampler (IOGenM StdGen) IO

-- | convenient type synonym to show specializations of Sampler
-- to particular pairs of monad and RNG
type SamplerST s = Sampler (STGenM StdGen s) (ST s)

instance StatefulGen g m => MonadDistribution (Sampler g m) where
  random = Sampler (ReaderT uniformDouble01M)

  uniform a b = Sampler (ReaderT $ uniformRM (a, b))
  normal m s = Sampler (ReaderT (MWC.normal m s))
  gamma shape scale = Sampler (ReaderT $ MWC.gamma shape scale)
  beta a b = Sampler (ReaderT $ MWC.beta a b)

  bernoulli p = Sampler (ReaderT $ MWC.bernoulli p)
  categorical ps = Sampler (ReaderT $ MWC.categorical ps)
  geometric p = Sampler (ReaderT $ MWC.geometric0 p)

-- | Sample with a random number generator of your choice e.g. the one
-- from `System.Random`.
--
-- >>> import Control.Monad.Bayes.Class
-- >>> import System.Random.Stateful hiding (random)
-- >>> newIOGenM (mkStdGen 1729) >>= sampleWith random
-- 4.690861245089605e-2
sampleWith :: StatefulGen g m => Sampler g m a -> g -> m a
sampleWith (Sampler m) = runReaderT m

-- | initialize random seed using system entropy, and sample
sampleIO, sampler :: SamplerIO a -> IO a
sampleIO x = initStdGen >>= newIOGenM >>= sampleWith x
sampler = sampleIO

-- | Run the sampler with a fixed random seed
sampleIOfixed :: SamplerIO a -> IO a
sampleIOfixed x = newIOGenM (mkStdGen 1729) >>= sampleWith x

-- | Run the sampler with a fixed random seed
sampleSTfixed :: SamplerST s b -> ST s b
sampleSTfixed x = newSTGenM (mkStdGen 1729) >>= sampleWith x

sampleMean :: [(Double, Log Double)] -> Double
sampleMean samples =
  let z = F.premap (ln . exp . snd) F.sum
      w = (F.premap (\(x, y) -> x * ln (exp y)) F.sum)
      s = (/) <$> w <*> z
   in F.fold s samples
