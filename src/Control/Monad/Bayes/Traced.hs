{-|
Module      : Control.Monad.Bayes.Traced
Description : Distributions on execution traces
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Traced (
  Traced,
  hoistT,
  marginal,
  mhStep,
  mh
) where

import Data.Bifunctor (second)
import Data.Monoid ((<>))
import Control.Monad (join)
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import qualified Data.Vector as V

import Numeric.Log (ln)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Free as FreeSampler

type Trace = [Double]

newtype Traced m a = Traced (m (Weighted (FreeSampler m) a, (Trace, a)))
runTraced :: Traced m a -> m (Weighted (FreeSampler m) a, (Trace, a))
runTraced (Traced c) = c

pushM :: Monad m => m (Weighted (FreeSampler m) a) -> Weighted (FreeSampler m) a
pushM = join . lift . lift

instance Monad m => Functor (Traced m) where
  fmap f (Traced c) = Traced $ do
    (m, t) <- c
    let m' = fmap f m
    let t' = second f t
    return (m', t')

instance Monad m => Applicative (Traced m) where
  pure x = Traced $ pure (pure x, ([], x))
  (Traced cf) <*> (Traced cx) = Traced $ do
    (mf, (tf, f)) <- cf
    (mx, (tx, x)) <- cx
    return (mf <*> mx, (tf <> tx, f x))

instance Monad m => Monad (Traced m) where
  (Traced cx) >>= f = Traced $ do
    (mx, (tx, x)) <- cx
    let m = mx >>= pushM . fmap fst . runTraced . f
    (_, (ty, y)) <- runTraced $ f x
    let t = tx <> ty
    return (m, (t, y))

instance MonadTrans Traced where
  lift m = Traced $ fmap ((,) (lift $ lift m) . (,) []) m

instance MonadSample m => MonadSample (Traced m) where
  random = Traced $ fmap ((,) random . \u -> ([u], u)) random

instance MonadCond m => MonadCond (Traced m) where
  score w = Traced $ fmap ((,) (score w)) (score w >> pure ([],()))

instance MonadInfer m => MonadInfer (Traced m)

hoistT :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistT f (Traced c) = Traced (f c)

marginal :: Monad m => Traced m a -> m a
marginal (Traced c) = fmap (snd . snd) c

mhTrans :: MonadSample m => Weighted (FreeSampler m) a -> (Trace, a) -> m (Trace, a)
mhTrans m (us,a) = do
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
  let xs = map snd ts
  return xs
