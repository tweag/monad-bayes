{-|
Module      : Control.Monad.Bayes.Traced.Basic
Description : Distributions on full execution traces of full programs
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Traced.Basic (
  Traced,
  hoistT,
  marginal,
  mhStep,
  mh
) where

import Data.Bifunctor (second)
import Data.Monoid ((<>))
import Data.Functor.Identity

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Free as FreeSampler

import Control.Monad.Bayes.Traced.Common(Trace, mhTrans')

data Traced m a = Traced (Weighted (FreeSampler Identity) a) (m (Trace, a))

traceDist :: Traced m a -> m (Trace, a)
traceDist (Traced _ d) = d

model :: Traced m a -> Weighted (FreeSampler Identity) a
model (Traced m _) = m

instance Monad m => Functor (Traced m) where
  fmap f (Traced m d) = Traced (fmap f m) (fmap (second f) d)

instance Monad m => Applicative (Traced m) where
  pure x = Traced (pure x) (pure ([],x))
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) ((\(t,x) (r,y) -> (t <> r, x y)) <$> df <*> dx)

instance Monad m => Monad (Traced m) where
  (Traced mx dx) >>= f = Traced my dy where
    my = mx >>= model . f
    dy = do
      (us, x) <- dx
      (vs, y) <- traceDist (f x)
      return (us <> vs, y)

instance MonadSample m => MonadSample (Traced m) where
  random = Traced random (fmap (\u -> ([u],u)) random)

instance MonadCond m => MonadCond (Traced m) where
  score w = Traced (score w) (score w >> pure ([],()))

instance MonadInfer m => MonadInfer (Traced m)

hoistT :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistT f (Traced m d) = Traced m (f d)

marginal :: Monad m => Traced m a -> m a
marginal (Traced _ d) = fmap snd d

mhStep :: MonadSample m => Traced m a -> Traced m a
mhStep (Traced m d) = Traced m d' where
  d' = d >>= mhTrans' m

mh :: MonadSample m => Int -> Traced m a -> m [a]
mh n (Traced m d) = fmap (map snd) t where
  t = f n
  f 0 = fmap (:[]) d
  f k = do
    x:xs <- f (k-1)
    y <- mhTrans' m x
    return (y:x:xs)
