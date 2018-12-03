{-|
Module      : Control.Monad.Bayes.Traced.Static
Description : Distributions on execution traces of full programs
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Traced.Static (
  Traced,
  hoistT,
  marginal,
  guided,
  mhStep,
  mh
) where

import Control.Monad.Trans
import Control.Applicative (liftA2)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Free as FreeSampler

import Control.Monad.Bayes.Traced.Common

-- | A tracing monad where only a subset of random choices are traced.
-- The random choices that are not to be traced should be lifted
-- from the transformed monad.
data Traced m a = Traced (Weighted (FreeSampler m) a) (m (Trace a))

traceDist :: Traced m a -> m (Trace a)
traceDist (Traced _ d) = d

model :: Traced m a -> Weighted (FreeSampler m) a
model (Traced m _) = m

instance Monad m => Functor (Traced m) where
  fmap f (Traced m d) = Traced (fmap f m) (fmap (fmap f) d)

instance Monad m => Applicative (Traced m) where
  pure x = Traced (pure x) (pure (pure x))
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) (liftA2 (<*>) df dx)

instance Monad m => Monad (Traced m) where
  (Traced mx dx) >>= f = Traced my dy where
    my = mx >>= model . f
    dy = dx `bind` (traceDist . f)

instance MonadTrans Traced where
  lift m = Traced (lift $ lift m) (fmap pure m)

instance MonadSample m => MonadSample (Traced m) where
  random = Traced random (fmap singleton random)

instance MonadCond m => MonadCond (Traced m) where
  score w = Traced (score w) (score w >> pure (scored w))

instance MonadInfer m => MonadInfer (Traced m)

hoistT :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistT f (Traced m d) = Traced m (f d)

marginal :: Monad m => Traced m a -> m a
marginal (Traced _ d) = fmap output d

guided :: MonadSample m => Traced m a -> Traced m b -> Traced m a
guided p q = Traced mp $ traceDist q >>= (withTrace mp) where
  mp = model p

importance :: MonadSample m => Traced m a -> Traced m b -> Traced m a
importance p q  = Traced mp $ traceDist q >>= (importanceWithTrace mp) where
  mp = model p

mhStep :: MonadSample m => Traced m a -> Traced m a
mhStep (Traced m d) = Traced m d' where
  d' = d >>= mhTrans m

mh :: MonadSample m => Int -> Traced m a -> m [a]
mh n (Traced m d) = fmap (map output) t where
  t = f n
  f 0 = fmap (:[]) d
  f k = do
    x:xs <- f (k-1)
    y <- mhTrans m x
    return (y:x:xs)
