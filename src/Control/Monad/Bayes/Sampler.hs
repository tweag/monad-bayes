{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts
 #-}

module Control.Monad.Bayes.Sampler (
    Sampler,
    sample,
    StdSampler,
    stdSample,
    MTSampler,
    mtSample
               ) where

import System.Random
import System.Random.MWC
import System.Random.Mersenne.Pure64
import Control.Monad (liftM2)
import qualified Data.Number.LogFloat as LogFloat
import Data.Tuple
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Categorical
import Data.Random.Distribution.Multinomial (Multinomial(Multinomial))
import Data.Random.Distribution
import qualified Data.Random.Sample
import Data.Random hiding (sample)
import Control.Arrow (first,second)
import Data.Number.LogFloat
import qualified Data.Foldable as Fold
import Control.Monad.State.Lazy

import Control.Monad.Bayes.Class

-- | A random sampler using `StdGen` as a source of randomness.
-- It uses `split` instead of passing the modified generator,
-- which makes `Sampler` lazy and potentially parallel.
-- Can be used for probabilistic computation, but not with conditioning,
-- unless transformed.
newtype Sampler a = Sampler (StdGen -> a)
    deriving (Functor)

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

instance MonadDist Sampler where
    discrete xs = wrapper $ fromWeightedList $
                    map (first fromLogFloat) $ zip xs [0..]
    normal m s  = wrapper $ Normal m s
    gamma a b   = wrapper $ Gamma a (1 / b)
    beta a b    = wrapper $ Beta a b  --need to check parameterization
    uniform a b = wrapper $ Uniform a b

-- | Wrapper for random-fu distributions.
wrapper :: Distribution d a => d a -> Sampler a
wrapper = Sampler . (fst .) . sampleState


---------------------------------------------------
-- MonadDist instance for the RVar monad and concrete samplers

instance MonadDist (RVarT m) where
  --should probably normalize before converting from log-domain
  discrete xs = rvarT $ fromWeightedList $ map (first fromLogFloat) $ zip xs [0..]
  normal m s = rvarT $ Normal m s
  gamma  a b = rvarT $ Gamma a (1 / b)
  beta   a b = rvarT $ Beta a b
  uniform a b = rvarT $ Uniform a b
  multinomial ps n = do
    let (xs,ws) = unzip ps
    let qs = map LogFloat.fromLogFloat $ map (/ LogFloat.sum ws) ws
    counts <- rvarT $ Multinomial qs n
    return $ zip xs counts

instance MonadDist IO where
    primitive d = convert $ primitive d where
        convert :: RVar a -> IO a
        convert = Data.Random.Sample.sample
    multinomial ps n = convert $ multinomial ps n where
      convert :: RVar a -> IO a
      convert = Data.Random.Sample.sample

newtype StdSampler a = StdSampler (State StdGen a)
    deriving (Functor, Applicative, Monad)
instance MonadDist StdSampler where
  primitive d = convert $ primitive d where
    convert :: RVar a -> StdSampler a
    convert = StdSampler . Data.Random.Sample.sample
  multinomial ps n = convert $ multinomial ps n where
    convert :: RVar a -> StdSampler a
    convert = StdSampler . Data.Random.Sample.sample

stdSample :: StdSampler a -> StdGen -> a
stdSample (StdSampler s) = evalState s

newtype MTSampler a = MTSampler (State PureMT a)
    deriving (Functor, Applicative, Monad)
instance MonadDist (MTSampler) where
    primitive d = convert $ primitive d where
        convert :: RVar a -> MTSampler a
        convert = MTSampler . Data.Random.Sample.sample
    multinomial ps n = convert $ multinomial ps n where
      convert :: RVar a -> MTSampler a
      convert = MTSampler . Data.Random.Sample.sample
mtSample :: MTSampler a -> PureMT -> a
mtSample (MTSampler s) = evalState s
