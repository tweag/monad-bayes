{-# LANGUAGE RankNTypes #-}

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

-- | Tracing monad that records random choices made in the program.
data Traced m n a = Traced
  { -- | Run the program with a modified trace.
    model :: Weighted (FreeSampler IdentityN) n a,
    -- | Record trace and output.
    traceDist :: (m n) (Trace n a)
  }

instance Monad (m n) => Functor (Traced m n) where
  fmap f (Traced m d) = Traced (fmap f m) (fmap (fmap f) d)

instance (RealFloat n, Monad (m n)) => Applicative (Traced m n) where
  pure x = Traced (pure x) (pure (pure x))
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) (liftA2 (<*>) df dx)

instance (Monad (m n), RealFloat n) => Monad (Traced m n) where
  (Traced mx dx) >>= f = Traced my dy
    where
      my = mx >>= model . f
      dy = dx `bind` (traceDist . f)

instance (MonadSample n m, RealFloat n) => MonadSample n (Traced m) where
  randomGeneric = Traced randomGeneric (fmap singleton randomGeneric)

instance (MonadCond n m, RealFloat n) => MonadCond n (Traced m) where
  scoreGeneric w = Traced (scoreGeneric w) (scoreGeneric w >> pure (scored w))

instance (RealFloat n, MonadInfer n m) => MonadInfer n (Traced m)

hoistT :: (forall x. m n x -> m n x) -> Traced m n a -> Traced m n a
hoistT f (Traced m d) = Traced m (f d)

-- | Discard the trace and supporting infrastructure.
marginal :: Monad (m n) => Traced m n a -> m n a
marginal (Traced _ d) = fmap output d

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: (MonadSample n m, RealFloat n) => Traced m n a -> Traced m n a
mhStep (Traced m d) = Traced m d'
  where
    d' = d >>= mhTrans' m

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps.
mh :: (MonadSample n m, RealFloat n) => Int -> Traced m n a -> m n [a]
mh n (Traced m d) = fmap (map output . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) d
      | otherwise = do
        (x :| xs) <- f (k - 1)
        y <- mhTrans' m x
        return (y :| x : xs)
