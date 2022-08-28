{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    hoist,
    marginal,
    mhStep,
    mh,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Free (FreeSampler)
import Control.Monad.Bayes.Traced.Common
  ( Trace (..),
    bind,
    mhTrans',
    scored,
    singleton,
  )
import Control.Monad.Bayes.Weighted (Weighted)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)
import Prelude hiding (Real)

-- | Tracing monad that records random choices made in the program.
data Traced m a = Traced
  { -- | Run the program with a modified trace.
    model :: Weighted (FreeSampler Identity) a,
    -- | Record trace and output.
    traceDist :: m (Trace (Real m) a)
  }

instance Monad m => Functor (Traced m) where
  fmap f (Traced m d) = Traced (fmap f m) (fmap (fmap f) d)

instance (Monad m, RealFloat (Real m)) => Applicative (Traced m) where
  pure x = Traced (pure x) (pure (pure x))
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) (liftA2 (<*>) df dx)

instance (Monad m, RealFloat (Real m)) => Monad (Traced m) where
  (Traced mx dx) >>= f = Traced my dy
    where
      my = mx >>= model . f
      dy = dx `bind` (traceDist . f)

instance MonadSample m => MonadSample (Traced m) where
  type Real (Traced m) = Real m

-- random = Traced random (fmap singleton random)

instance (MonadCond m, RealFloat (Real m)) => MonadCond (Traced m) where
  score w = undefined -- Traced (score w) (score w >> pure (scored w))

instance MonadInfer m => MonadInfer (Traced m)

hoist :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoist f (Traced m d) = Traced m (f d)

-- | Discard the trace and supporting infrastructure.
marginal :: Monad m => Traced m a -> m a
marginal (Traced _ d) = fmap output d

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: MonadSample m => Traced m a -> Traced m a
mhStep (Traced m d) = Traced m d'
  where
    d' = d >>= mhTrans' m

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps.
mh :: MonadSample m => Int -> Traced m a -> m [a]
mh n (Traced m d) = fmap (map output . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) d
      | otherwise = do
        (x :| xs) <- f (k - 1)
        y <- mhTrans' m x
        return (y :| x : xs)
