{-|
Module      : Control.Monad.Bayes.Traced
Description : Distributions on execution traces
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  UndecidableInstances
 #-}

module Control.Monad.Bayes.Traced (
  Traced,
  hoistT,
  marginal,
  mhStep,
  mh
) where

import Data.Bifunctor (second)
import Data.Monoid ((<>))
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import qualified Data.Vector as V

import Numeric.Log (ln)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Free as FreeSampler

type Trace = [Double]

data Traced m a = Traced (Weighted (FreeSampler m) a) (m (Trace, a))

traceDist :: Traced m a -> m (Trace, a)
traceDist (Traced _ d) = d

model :: Traced m a -> Weighted (FreeSampler m) a
model (Traced m _) = m

instance Monad m => Functor (Traced m) where
  fmap f (Traced m d) = Traced (fmap f m) (fmap (second f) d)

instance Monad m => Applicative (Traced m) where
  pure x = Traced (pure x) (pure ([],x))
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) ((\(t,x) (r,y) -> (t <> r, x y)) <$> df <*> dx)

instance Monad m => Monad (Traced m) where
  (Traced mx dx) >>= f = Traced my dy where
    dy = do
      (us, x) <- dx
      (vs, y) <- traceDist $ f x
      return (us <> vs, y)
    my = mx >>= model . f

instance MonadTrans Traced where
  lift m = Traced (lift $ lift m) (fmap (\x -> ([],x)) m)

instance MonadSample m => MonadSample (Traced m) where
  random = Traced random (fmap (\u -> ([u],u)) random)

instance MonadCond m => MonadCond (Traced m) where
  score w = Traced (score w) (score w >> pure ([],()))

instance MonadInfer m => MonadInfer (Traced m)

hoistT :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistT f (Traced m d) = Traced m (f d)

marginal :: Monad m => Traced m a -> m a
marginal (Traced _ d) = fmap snd d

mhTrans :: MonadSample m => Weighted (FreeSampler m) a -> (Trace, a) -> m (Trace, a)
mhTrans m (us,a) = do
  -- TODO: Cache the weight so that we don't need to recompute it here.
  (_, p) <- runWeighted $ Weighted.hoist (withRandomness us) m
  us' <- do
    let n = length us
    i <- categorical $ V.replicate n (1 / fromIntegral n)
    u' <- random
    let (xs, _:ys) = splitAt i us
    return $ xs ++ (u':ys)
  ((b, q), vs) <- runWriterT $ runWeighted $ Weighted.hoist (WriterT . withPartialRandomness us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral (length vs) / p * fromIntegral (length us))
  accept <- bernoulli ratio
  return $ if accept then (vs,b) else (us,a)

mhStep :: MonadSample m => Traced m a -> Traced m a
mhStep (Traced m d) = Traced m d' where
  d' = d >>= mhTrans m

mh :: MonadSample m => Int -> Traced m a -> m [a]
-- Note that this re-runs the simulation for each trace produced.
mh n (Traced m d) = fmap (map snd) t where
  t = f n
  f 0 = fmap (:[]) d
  f k = do
    x:xs <- f (k-1)
    y <- mhTrans m x
    return (y:x:xs)
