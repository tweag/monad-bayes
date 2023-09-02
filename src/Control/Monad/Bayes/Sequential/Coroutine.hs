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
module Control.Monad.Bayes.Sequential.Coroutine
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
  ( MonadDistribution,
    MonadFactor (..),
    MonadMeasure,
    MonadMeasureTrans (..),
  )
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
newtype Sequential m a = Sequential {runSequential :: Coroutine (Await ()) m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

extract :: Await () a -> a
extract (Await f) = f ()

deriving via (MonadMeasureTrans Sequential m) instance MonadDistribution m => MonadDistribution (Sequential m)

-- | Execution is 'suspend'ed after each 'score'.
instance MonadFactor m => MonadFactor (Sequential m) where
  score w = lift (score w) >> suspend

instance MonadMeasure m => MonadMeasure (Sequential m)

-- | A point where the computation is paused.
suspend :: Monad m => Sequential m ()
suspend = Sequential await

-- | Remove the remaining suspension points.
finish :: Monad m => Sequential m a -> m a
finish = pogoStick extract . runSequential

-- | Execute to the next suspension point.
-- If the computation is finished, do nothing.
--
-- > finish = finish . advance
advance :: Monad m => Sequential m a -> Sequential m a
advance = Sequential . bounce extract . runSequential

-- | Return True if no more suspension points remain.
finished :: Monad m => Sequential m a -> m Bool
finished = fmap isRight . resume . runSequential

-- | Transform the inner monad.
-- This operation only applies to computation up to the first suspension.
hoistFirst :: (forall x. m x -> m x) -> Sequential m a -> Sequential m a
hoistFirst f = Sequential . Coroutine . f . resume . runSequential

-- | Transform the inner monad.
-- The transformation is applied recursively through all the suspension points.
hoist ::
  (Monad m, Monad n) =>
  (forall x. m x -> n x) ->
  Sequential m a ->
  Sequential n a
hoist f = Sequential . mapMonad f . runSequential

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

-- | Sequential importance sampling.
-- Applies a given transformation after each time step.
sequentially,
  sis ::
    Monad m =>
    -- | transformation
    (forall x. m x -> m x) ->
    -- | number of time steps
    Int ->
    Sequential m a ->
    m a
sequentially f k = finish . composeCopies k (advance . hoistFirst f)

-- | synonym
sis = sequentially
