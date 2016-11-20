{-|
Module      : Control.Monad.Bayes.Sampler
Description : Psuedo-random sampling monads
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts,
  TypeFamilies
 #-}

module Control.Monad.Bayes.Sampler (
    SamplerIO,
    sampleIO,
    Sampler,
    sample,
    StdSampler,
    stdSample,
    MTSampler,
    mtSample
               ) where

import System.Random
import System.Random.MWC (GenIO, createSystemRandom, uniformR)
import qualified System.Random.MWC.Distributions as MWC
import System.Random.Mersenne.Pure64
import Control.Monad (liftM2)
import Data.Vector (fromList)
import Data.Tuple
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Categorical
import Data.Random.Distribution.Multinomial (Multinomial(Multinomial))
import Data.Random.Distribution
import qualified Data.Random.Sample
import Data.Random hiding (sample)
import Control.Arrow (first,second)
import qualified Data.Foldable as Fold
import Control.Monad.State.Lazy
import Control.Monad.Trans.Reader

import Control.Monad.Bayes.Class

-- | An `IO` based random sampler using the MWC-Random package.
newtype SamplerIO a = SamplerIO (ReaderT GenIO IO a)
  deriving(Functor, Applicative, Monad)

-- | Initialize PRNG using OS-supplied randomness.
-- For efficiency this operation should be applied at the very end, ideally once per program.
sampleIO :: SamplerIO a -> IO a
sampleIO (SamplerIO m) = createSystemRandom >>= runReaderT m

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



-- | A random sampler using `StdGen` as a source of randomness.
-- It uses `split` instead of passing the modified generator,
-- which makes `Sampler` lazy and potentially parallel,
-- but strictly speaking not associative.
newtype Sampler a = Sampler (StdGen -> a)
    deriving (Functor)

-- | Samples a value using the supplied RNG.
sample :: Sampler a -> StdGen -> a
sample (Sampler s) = s

-- | Converts from `State` `StdGen` to `Sampler`.
fromState :: Control.Monad.State.Lazy.State System.Random.StdGen a -> Sampler a
fromState = Sampler . (fst .) . runState

instance Applicative Sampler where
    pure = return
    (<*>) = liftM2 ($)

instance Monad Sampler where
    return = Sampler . const
    Sampler d >>= f = Sampler $
                       \g -> let
                           x          = d g1
                           Sampler y  = f x
                           (g1,g2)    = split g
         in
           y g2

type instance CustomReal Sampler = Double

instance MonadDist Sampler where
  discrete xs = wrapper $ fromWeightedList $ zip xs (map fromIntegral [0..])
  normal m s  = fmap realToFrac $ wrapper $ Normal m s
  gamma a b   = fmap realToFrac $ wrapper $ Gamma a (recip b)
  beta a b    = fmap realToFrac $ wrapper $ Beta a b
  uniform a b = fmap realToFrac $ wrapper $ Uniform a b

-- | Wrapper for random-fu distributions.
wrapper :: Distribution d a => d a -> Sampler a
wrapper = Sampler . (fst .) . sampleState


---------------------------------------------------
-- MonadDist instance for the RVar monad and concrete samplers

type instance CustomReal (RVarT m) = Double

instance MonadDist (RVarT m) where
  discrete xs = rvarT $ fromWeightedList $ zip xs (map fromIntegral [0..])
  normal  m s = fmap realToFrac $ rvarT $ Normal m s
  gamma   a b = fmap realToFrac $ rvarT $ Gamma a (recip b)
  beta    a b = fmap realToFrac $ rvarT $ Beta a b
  uniform a b = fmap realToFrac $ rvarT $ Uniform a b
  -- multinomial ps n = do
  --   let (xs,ws) = unzip ps
  --   let qs = map LogFloat.fromLogFloat $ map (/ LogFloat.sum ws) ws
  --   counts <- rvarT $ Multinomial qs n
  --   return $ zip xs counts

type instance CustomReal StdSampler = Double

-- | A sampler that uses `StdGen` to generate random numbers.
-- Unlike `Sampler`, it passes the generator using a state monad,
-- so it satisfies the monad laws but is not lazy.
newtype StdSampler a = StdSampler (State StdGen a)
    deriving (Functor, Applicative, Monad)
instance MonadDist StdSampler where
  primitive d = convert $ primitive d where
    convert :: RVar a -> StdSampler a
    convert = StdSampler . Data.Random.Sample.sample
  multinomial ps n = convert $ multinomial ps n where
    convert :: RVar a -> StdSampler a
    convert = StdSampler . Data.Random.Sample.sample

-- | Samples a value using the supplied RNG.
stdSample :: StdSampler a -> StdGen -> a
stdSample (StdSampler s) = evalState s

type instance CustomReal MTSampler = Double

-- | Similar to `StdSampler`, but uses a Mersenne Twister instead.
newtype MTSampler a = MTSampler (State PureMT a)
    deriving (Functor, Applicative, Monad)
instance MonadDist (MTSampler) where
  primitive d = convert $ primitive d where
      convert :: RVar a -> MTSampler a
      convert = MTSampler . Data.Random.Sample.sample
  multinomial ps n = convert $ multinomial ps n where
    convert :: RVar a -> MTSampler a
    convert = MTSampler . Data.Random.Sample.sample

-- | Samples a value using the supplied RNG.
mtSample :: MTSampler a -> PureMT -> a
mtSample (MTSampler s) = evalState s
