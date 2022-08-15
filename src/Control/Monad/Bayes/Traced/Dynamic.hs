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
    hoist,
    marginal,
    freeze,
    mhStep,
    mh,
  )
where

import Control.Monad (join)
import Control.Monad.Bayes.Class
  ( MonadCond (..),
    MonadInfer,
    MonadSample (random),
  )
import Control.Monad.Bayes.Density.Free (Density)
import Control.Monad.Bayes.Traced.Common
  ( Trace (..),
    bind,
    mhTrans,
    scored,
    singleton, mhTransFree
  )
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Trans (MonadTrans (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)

-- | A tracing monad where only a subset of random choices are traced and this
-- subset can be adjusted dynamically.
newtype Traced m a = Traced {runTraced :: m (Weighted (Density m) a, Trace a)}

pushM :: Monad m => m (Weighted (Density m) a) -> Weighted (Density m) a
pushM = join . lift . lift

instance Monad m => Functor (Traced m) where
  fmap f (Traced c) = Traced $ do
    (m, t) <- c
    let m' = fmap f m
    let t' = fmap f t
    return (m', t')

instance Monad m => Applicative (Traced m) where
  pure x = Traced $ pure (pure x, pure x)
  (Traced cf) <*> (Traced cx) = Traced $ do
    (mf, tf) <- cf
    (mx, tx) <- cx
    return (mf <*> mx, tf <*> tx)

instance Monad m => Monad (Traced m) where
  (Traced cx) >>= f = Traced $ do
    (mx, tx) <- cx
    let m = mx >>= pushM . fmap fst . runTraced . f
    t <- return tx `bind` (fmap snd . runTraced . f)
    return (m, t)

instance MonadTrans Traced where
  lift m = Traced $ fmap ((,) (lift $ lift m) . pure) m

instance MonadSample m => MonadSample (Traced m) where
  random = Traced $ fmap ((,) random . singleton) random

instance MonadCond m => MonadCond (Traced m) where
  score w = Traced $ fmap (score w,) (score w >> pure (scored w))

instance MonadInfer m => MonadInfer (Traced m)

hoist :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoist f (Traced c) = Traced (f c)

-- | Discard the trace and supporting infrastructure.
marginal :: Monad m => Traced m a -> m a
marginal (Traced c) = fmap (output . snd) c

-- | Freeze all traced random choices to their current values and stop tracing
-- them.
freeze :: Monad m => Traced m a -> Traced m a
freeze (Traced c) = Traced $ do
  (_, t) <- c
  let x = output t
  return (return x, pure x)

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: MonadSample m => Traced m a -> Traced m a
mhStep (Traced c) = Traced $ do
  (m, t) <- c
  t' <- mhTransFree m t
  return (m, t')

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps.
mh :: MonadSample m => Int -> Traced m a -> m [a]
mh n (Traced c) = do
  (m, t) <- c
  let f k
        | k <= 0 = return (t :| [])
        | otherwise = do
          (x :| xs) <- f (k - 1)
          y <- mhTransFree m x
          return (y :| x : xs)
  fmap (map output . NE.toList) (f n)
