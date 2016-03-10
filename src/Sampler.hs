{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts
 #-}

module Sampler (
    Sampler,
    sample
               ) where

import System.Random
import System.Random.MWC
import System.Random.Mersenne.Pure64
import Control.Monad (liftM2)
import Data.Tuple
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Categorical
import Data.Random.Distribution
import qualified Data.Random.Sample
import Data.Random hiding (sample)
import Control.Arrow (first,second)
import Data.Number.LogFloat
import qualified Data.Foldable as Fold
import Control.Monad.State.Lazy

import Base

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
    categorical xs = wrapper $ fromWeightedList $
                     map (first fromLogFloat) $ map swap $ Fold.toList xs
    normal m s = wrapper $ Normal m s
    gamma a b  = wrapper $ Gamma a (1 / b)
    beta a b   = wrapper $ Beta a b  --need to check parameterization

-- | Wrapper for random-fu distributions.
wrapper :: Distribution d a => d a -> Sampler a
wrapper = Sampler . (fst .) . sampleState


---------------------------------------------------
-- MonadDist instance for the RVar monad and concrete samplers

instance MonadDist (RVarT m) where
  --should probably normalize before converting from log-domain
  categorical xs = rvarT $ fromWeightedList $ map (first fromLogFloat . swap) $
                   Fold.toList xs
  normal m s = rvarT $ Normal m s
  gamma  a b = rvarT $ Gamma a (1 / b)
  beta   a b = rvarT $ Beta a b

instance MonadDist IO where
    primitive d = convert $ primitive d where
        convert :: RVar a -> IO a
        convert = Data.Random.Sample.sample

instance Monad m => MonadDist (StateT StdGen m) where
    primitive d = convert $ primitive d where
        convert :: Monad m => RVar a -> StateT StdGen m a
        convert = Data.Random.Sample.sample

instance Monad m => MonadDist (StateT PureMT m) where
    primitive d = convert $ primitive d where
        convert :: Monad m => RVar a -> StateT PureMT m a
        convert = Data.Random.Sample.sample
