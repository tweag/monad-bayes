-- |
-- Module      : Control.Monad.Bayes.Traced.Basic
-- Description : Distributions on full execution traces of full programs
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Basic
  ( Traced,
    hoistT,
    marginal,
    mhStep,
    mh,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Free as FreeSampler
import Control.Monad.Bayes.Traced.Common
import Control.Monad.Bayes.Weighted as Weighted
import Data.Functor.Identity

-- | Tracing monad that records random choices made in the program.
-- The first component is used to run the program with a modified trace,
-- while the second records a trace and an output value from a run.
data Traced m a = Traced (Weighted (FreeSampler Identity) a) (m (Trace a))

traceDist :: Traced m a -> m (Trace a)
traceDist (Traced _ d) = d

model :: Traced m a -> Weighted (FreeSampler Identity) a
model (Traced m _) = m

instance Monad m => Functor (Traced m) where
  fmap f (Traced m d) = Traced (fmap f m) (fmap (fmap f) d)

instance Monad m => Applicative (Traced m) where
  pure x = Traced (pure x) (pure (pure x))
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) (liftA2 (<*>) df dx)

instance Monad m => Monad (Traced m) where
  (Traced mx dx) >>= f = Traced my dy
    where
      my = mx >>= model . f
      dy = dx `bind` (traceDist . f)

instance MonadSample m => MonadSample (Traced m) where
  random = Traced random (fmap singleton random)

instance MonadCond m => MonadCond (Traced m) where
  score w = Traced (score w) (score w >> pure (scored w))

instance MonadInfer m => MonadInfer (Traced m)

hoistT :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistT f (Traced m d) = Traced m (f d)

-- | Discard the trace and supporting infrastructure.
marginal :: Monad m => Traced m a -> m a
marginal (Traced _ d) = fmap output d

-- | A single step of the Trace MH algorithm.
mhStep :: MonadSample m => Traced m a -> Traced m a
mhStep (Traced m d) = Traced m d'
  where
    d' = d >>= mhTrans' m

-- | Full run of the Trace MH algorithm with a specified
-- number of steps.
mh :: MonadSample m => Int -> Traced m a -> m [a]
mh n (Traced m d) = fmap (map output) t
  where
    t = f n
    f 0 = fmap (: []) d
    f k = do
      ~(x : xs) <- f (k -1)
      y <- mhTrans' m x
      return (y : x : xs)
