{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Control.Monad.Bayes.Free
-- Description : Free monad transformer over random sampling
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- 'FreeSampler' is a free monad transformer over random sampling.
module Control.Monad.Bayes.Free
  ( FreeSampler (..),
    hoist,
    interpret,
    withRandomness,
    withPartialRandomness,
    runWith,
  )
where

import Control.Monad.Bayes.Class
import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (FT, MonadFree (..), hoistFT, iterT, iterTM, liftF)
import Control.Monad.Writer (WriterT (..), tell)
import Data.Functor.Identity (Identity, runIdentity)
import Debug.Trace (trace, traceM)

-- | Random sampling functor.
newtype SamF n a = Random (n -> a)

instance Functor (SamF n) where
  fmap f (Random k) = Random (f . k)

-- | Free monad transformer over random sampling.
--
-- Uses the Church-encoded version of the free monad for efficiency.
newtype FreeSampler m n a = FreeSampler {runFreeSampler :: FT (SamF n) (m n) a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadFree (SamF n) (FreeSampler m n) where
  wrap = FreeSampler . wrap . fmap runFreeSampler

instance (Monad (m n), RealFloat n) => MonadSample n (FreeSampler m) where
  randomGeneric = FreeSampler $ liftF (Random id)

-- | Hoist 'FreeSampler' through a monad transform.
hoist ::
  (Monad (m n), Monad (m' n)) =>
  (forall x. (m n) x -> (m' n) x) ->
  FreeSampler m n a ->
  FreeSampler m' n a
hoist f (FreeSampler m) = FreeSampler (hoistFT f m)

-- | Execute random sampling in the transformed monad.
interpret :: MonadSample n m => FreeSampler m n a -> m n a
interpret (FreeSampler m) = iterT f m
  where
    f (Random k) = randomGeneric >>= k

-- | Execute computation with supplied values for random choices.
withRandomness :: (Monad (m n), Show n) => [n] -> FreeSampler m n a -> m n a
withRandomness randomness (FreeSampler m) = evalStateT (iterTM f m) randomness
  where
    f (Random k) = do
      xs <- get
      -- traceM (show xs <> " P")
      case trace (show xs <> " P" <> show (length xs) ) xs of
        [] -> error "FreeSampler: the list of randomness was too short"
        y : ys -> put (traceIt ys) >> k y

traceIt x = trace (show x <> " L") x
-- | Execute computation with supplied values for a subset of random choices.
-- Return the output value and a record of all random choices used, whether
-- taken as input or drawn using the transformed monad.
withPartialRandomness :: MonadSample n m => [n] -> FreeSampler m n a -> m n (a, [n])
withPartialRandomness randomness (FreeSampler m) =
  runWriterT $ evalStateT (iterTM (undefined) $ hoistFT lift m) randomness
  where
    f (Random k) = do
      -- This block runs in StateT [n] (WriterT [n]) m.
      -- StateT propagates consumed randomness while WriterT records
      -- randomness used, whether old or new.
      xs <- get
      x <- case xs of
        [] -> randomGeneric
        y : ys -> put ys >> return y
      tell [x]
      k x

-- | Like 'withPartialRandomness', but use an arbitrary sampling monad.
-- runWith :: MonadSample n m => [n] -> FreeSampler n Identity a -> m (a, [n])
runWith :: MonadSample n m => [n] -> FreeSampler IdentityN n a -> m n (a, [n])
runWith randomness m = withPartialRandomness randomness $ hoist (return . runIdentityN) m
