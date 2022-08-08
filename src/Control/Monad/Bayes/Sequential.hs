{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Control.Monad.Bayes.Sequential
-- Description : Suspendable probabilistic computation
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- 'Sequential' represents a computation that can be suspended.
module Control.Monad.Bayes.Sequential
  ( Sequential,
    suspend,
    finish,
    advance,
    finished,
    hoistFirst,
    hoist,
    sequentially,
    sis,
  )
where

import Control.Monad.Bayes.Class
import Control.Monad.Coroutine
  ( Coroutine (..),
    bounce,
    mapMonad,
    pogoStick,
  )
import Control.Monad.Coroutine.SuspensionFunctors
  ( Await (..),
    await,
  )
import Control.Monad.Trans (MonadIO, MonadTrans (..))
import Data.Either (isRight)

-- | Represents a computation that can be suspended at certain points.
-- The intermediate monadic effects can be extracted, which is particularly
-- useful for implementation of Sequential Monte Carlo related methods.
-- All the probabilistic effects are lifted from the transformed monad, but
-- also `suspend` is inserted after each `factor`.
newtype Sequential m n a = Sequential {runSequential :: Coroutine (Await ()) (m n) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

extract :: Await () a -> a
extract (Await f) = f ()

instance MonadSample n m => MonadSample n (Sequential m) where
  randomGeneric = Sequential $ lift randomGeneric
  bernoulli = Sequential . lift . bernoulli
  categorical = Sequential . lift . categorical

-- | Execution is 'suspend'ed after each 'score'.
instance MonadCond n m => MonadCond n (Sequential m) where
  scoreGeneric w = Sequential (lift (scoreGeneric w)) >> suspend

instance MonadInfer n m => MonadInfer n (Sequential m)

-- | A point where the computation is paused.
suspend :: Monad (m n) => Sequential m n ()
suspend = Sequential await

-- | Remove the remaining suspension points.
finish :: Monad (m n) => Sequential m n a -> m n a
finish = pogoStick extract . runSequential

-- | Execute to the next suspension point.
-- If the computation is finished, do nothing.
--
-- > finish = finish . advance
advance :: Monad (m n) => Sequential m n a -> Sequential m n a
advance = Sequential . bounce extract . runSequential

-- | Return True if no more suspension points remain.
finished :: Monad (m n) => Sequential m n a -> m n Bool
finished = fmap isRight . resume . runSequential

-- | Transform the inner monad.
-- This operation only applies to computation up to the first suspension.
hoistFirst :: (forall x. m n x -> m n x) -> Sequential m n a -> Sequential m n a
hoistFirst f = Sequential . Coroutine . f . resume . runSequential

-- | Transform the inner monad.
-- The transformation is applied recursively through all the suspension points.
hoist ::
  (Monad (m n), Monad (m' n)) =>
  (forall x. m n x -> m' n x) ->
  Sequential m n a ->
  Sequential m' n a
hoist f = Sequential . mapMonad f . runSequential

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

-- | Sequential importance sampling.
-- Applies a given transformation after each time step.
sequentially,
  sis ::
    Monad (m n) =>
    -- | transformation
    (forall x. m n x -> m n x) ->
    -- | number of time steps
    Int ->
    Sequential m n a -> m a
sequentially f k = finish . composeCopies k (advance . hoistFirst f)

-- | deprecated synonym
sis = sequentially
