{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Dynamic
-- Description : Distributions on execution traces that can be dynamically frozen
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Dynamic
  ( Traced,
    hoistT,
    marginal,
    freeze,
    mhStep,
    mh,
  )
where

import Control.Monad (join)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Free
import Control.Monad.Bayes.Traced.Common
  ( Trace (..),
    bind,
    mhTrans,
    scored,
    singleton,
  )
import Control.Monad.Bayes.Weighted
import Control.Monad.Trans (MonadTrans (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)

-- | A tracing monad where only a subset of random choices are traced and this
-- subset can be adjusted dynamically.
newtype Traced m n a = Traced {runTraced :: m n (Weighted (FreeSampler m) n a, Trace n a)}

pushM ::
  Monad (m n) =>
  m n (Weighted (FreeSampler m) n a) ->
  Weighted (FreeSampler m) n a
pushM = join . Weighted . lift . FreeSampler . lift

instance Monad (m n) => Functor (Traced m n) where
  fmap f (Traced c) = Traced $ do
    (m, t) <- c
    let m' = fmap f m
    let t' = fmap f t
    return (m', t')

instance (RealFloat n, Monad (m n)) => Applicative (Traced m n) where
  pure x = Traced $ pure (pure x, pure x)
  (Traced cf) <*> (Traced cx) = Traced $ do
    (mf, tf) <- cf
    (mx, tx) <- cx
    return (mf <*> mx, tf <*> tx)

instance (Monad (m n), RealFloat n) => Monad (Traced m n) where
  (Traced cx) >>= f = Traced $ do
    (mx, tx) <- cx
    let m = mx >>= pushM . fmap fst . runTraced . f
    t <- return tx `bind` (fmap snd . runTraced . f)
    return (m, t)

-- instance RealFloat n => MonadTrans (Traced n) where
--   lift m = Traced $ fmap ((,) (lift $ lift m) . pure) m

instance (MonadSample n m, RealFloat n) => MonadSample n (Traced m) where
  randomGeneric = Traced $ fmap ((,) randomGeneric . singleton) randomGeneric

instance (MonadCond n m, RealFloat n) => MonadCond n (Traced m) where
  scoreGeneric w = Traced $ fmap (scoreGeneric w,) (scoreGeneric w >> pure (scored w))

instance (RealFloat n, MonadInfer n m) => MonadInfer n (Traced m)

hoistT :: (forall x. m n x -> m n x) -> Traced m n a -> Traced m n a
hoistT f (Traced c) = Traced (f c)

-- | Discard the trace and supporting infrastructure.
marginal :: Monad (m n) => Traced m n a -> m n a
marginal (Traced c) = fmap (output . snd) c

-- | Freeze all traced random choices to their current values and stop tracing
-- them.
freeze :: (RealFloat n, Monad (m n)) => Traced m n a -> Traced m n a
freeze (Traced c) = Traced $ do
  (_, t) <- c
  let x = output t
  return (return x, pure x)

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: (MonadSample n m, RealFloat n) => Traced m n a -> Traced m n a
mhStep (Traced c) = Traced $ do
  (m, t) <- c
  t' <- mhTrans m t
  return (m, t')

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps.
mh :: (MonadSample n m, RealFloat n) => Int -> Traced m n a -> m n [a]
mh n (Traced c) = do
  (m, t) <- c
  let f k
        | k <= 0 = return (t :| [])
        | otherwise = do
          (x :| xs) <- f (k - 1)
          y <- mhTrans m x
          return (y :| x : xs)
  fmap (map output . NE.toList) (f n)
