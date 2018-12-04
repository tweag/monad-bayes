{-|
Module      : Control.Monad.Bayes.Traced.Dynamic
Description : Distributions on execution traces that can be dynamically frozen
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Traced.Dynamic (
  Traced,
  hoistT,
  marginal,
  guided,
  importance,
  freeze,
  mhStep,
  mh
) where

import Control.Monad (join)
import Control.Monad.Trans

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Free as FreeSampler

import Control.Monad.Bayes.Traced.Common

-- | A tracing monad where only a subset of random choices are traced
-- and this subset can be adjusted dynamically.
newtype Traced m a = Traced (m (Weighted (FreeSampler m) a, Trace a))
runTraced :: Traced m a -> m (Weighted (FreeSampler m) a, Trace a)
runTraced (Traced c) = c

pushM :: Monad m => m (Weighted (FreeSampler m) a) -> Weighted (FreeSampler m) a
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
  score w = Traced $ fmap ((,) (score w)) (score w >> pure (scored w))

instance MonadInfer m => MonadInfer (Traced m)

hoistT :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistT f (Traced c) = Traced (f c)

marginal :: Monad m => Traced m a -> m a
marginal (Traced c) = fmap (output . snd) c

guided :: MonadSample m => Traced m a -> Traced m b -> Traced m a
guided p q = Traced $ do
  (mp, _) <- runTraced p
  (_, tq) <- runTraced q
  tp' <- withTrace mp tq
  return (mp, tp')

importance :: MonadSample m => Traced m a -> Traced m b -> Traced m a
importance p q = Traced $ do
  (mp, _) <- runTraced p
  (_, tq) <- runTraced q
  tp' <- importanceWithTrace mp tq
  return (mp, tp')

-- | Freeze all traced random choices to their current
-- values and stop tracing them.
freeze :: Monad m => Traced m a -> Traced m a
freeze (Traced c) = Traced $ do
  (_, t) <- c
  let x = output t
  return (return x, (pure x))

mhStep :: MonadSample m => Traced m a -> Traced m a
mhStep (Traced c) = Traced $ do
  (m, t) <- c
  t' <- mhTrans m t
  return (m, t')

mh :: MonadSample m => Int -> Traced m a -> m [a]
mh n (Traced c) = do
  (m,t) <- c
  let f 0 = return [t]
      f k = do
        x:xs <- f (k-1)
        y <- mhTrans m x
        return (y:x:xs)
  ts <- f n
  let xs = map output ts
  return xs
