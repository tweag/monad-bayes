
module Sampler where

import Base
import Explicit (Explicit)

import Data.Random.Distribution.Beta (Beta(Beta))
import Data.Random.Distribution.Exponential (Exponential(Exp))
import qualified Data.Random as Ext
import Control.Arrow (first, second)
import Control.Applicative (Applicative, pure, (<*>))
import System.Random (StdGen, split)

-- | A basic probability monad for sampling from tractable distributions.
-- Does not support conditioning.
newtype StdSampler a = StdSampler {run :: StdGen -> a}

instance Functor StdSampler where
    fmap f (StdSampler x) = StdSampler (f . x)

instance Applicative StdSampler where
    pure x = StdSampler (\_ -> x)
    (StdSampler f) <*> (StdSampler x) =
        StdSampler (uncurry ($) . first f . second x . split)

instance Monad StdSampler where
    return = pure
    (StdSampler x) >>= f =
      StdSampler (uncurry ($) . first (run . f . x) . split)

instance DiscreteDist StdSampler where
    categorical = sampler . (categorical :: [(a,Prob)] -> Explicit a)

instance ContinuousDist StdSampler where
    normal m s    = external $ Ext.Normal m s
    gamma  k t    = external $ Ext.Gamma  k t
    beta   a b    = external $ Beta   a b
    exponential l = external $ Exp    l

instance Sampler StdSampler where
    sampler = StdSampler . flip sample


instance Sampleable StdSampler where
    sample = flip run

-- | A wrapper for externally defined distributions.
external :: (Ext.Distribution d a, Sampler s) => d a -> s a
external d = sampler $ StdSampler (fst . Ext.sampleState d)
