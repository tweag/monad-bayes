{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Static
-- Description : Distributions on execution traces of full programs
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Static
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
    mhTrans,
    scored,
    singleton,
  )
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Trans (MonadTrans (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)

-- | A tracing monad where only a subset of random choices are traced.
--
-- The random choices that are not to be traced should be lifted from the
-- transformed monad.
data Traced m n a = Traced
  { model :: Weighted (FreeSampler m) n a,
    traceDist :: m n (Trace n a)
  }

instance Monad (m n) => Functor (Traced m n) where
  fmap f (Traced m d) = Traced (fmap f m) (fmap (fmap f) d)

instance (RealFloat n, Monad (m n)) => Applicative (Traced m n) where
  pure x = Traced (pure x) (pure (pure x))
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) (liftA2 (<*>) df dx)

instance (RealFloat n, Monad (m n)) => Monad (Traced m n) where
  (Traced mx dx) >>= f = Traced my dy
    where
      my = mx >>= model . f
      dy = dx `bind` (traceDist . f)

-- instance RealFloat n => MonadTrans (Traced n) where
--   lift m = Traced (lift $ lift m) (fmap pure m)

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
    d' = d >>= mhTrans m

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps. Newest samples are at the head of the list.
mh :: (MonadSample n m, RealFloat n) => Int -> Traced m n a -> m n [a]
mh n (Traced m d) = fmap (map output . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) d
      | otherwise = do
        (x :| xs) <- f (k - 1)
        y <- mhTrans m x
        return (y :| x : xs)
