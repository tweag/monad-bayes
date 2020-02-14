{-|
Module      : Control.Monad.Bayes.Sequential
Description : Suspendable probabilistic computation
Copyright   : (c) Adam Scibior, 2015-2020
License     : MIT
Maintainer  : leonhard.markert@tweag.io
Stability   : experimental
Portability : GHC

'Sequential' represents a computation that can be suspended.
-}

module Control.Monad.Bayes.Sequential (
    Sequential,
    suspend,
    finish,
    advance,
    finished,
    hoistFirst,
    hoist,
    sis
                ) where

import Control.Monad.Trans
import Control.Monad.Coroutine hiding (suspend)
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Either

import Control.Monad.Bayes.Class

-- | Represents a computation that can be suspended at certain points.
-- The intermediate monadic effects can be extracted, which is particularly
-- useful for implementation of Sequential Monte Carlo related methods.
-- All the probabilistic effects are lifted from the transformed monad, but
-- also `suspend` is inserted after each `factor`.
newtype Sequential m a = Sequential {runSequential :: Coroutine (Await ()) m a}
  deriving(Functor,Applicative,Monad,MonadTrans,MonadIO)

extract :: Await () a -> a
extract (Await f) = f ()

instance MonadSample m => MonadSample (Sequential m) where
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical

-- | Execution is 'suspend'ed after each 'score'.
instance MonadCond m => MonadCond (Sequential m) where
  score w = lift (score w) >> suspend

instance MonadInfer m => MonadInfer (Sequential m)

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
hoist :: (Monad m, Monad n) =>
            (forall x. m x -> n x) -> Sequential m a -> Sequential n a
hoist f = Sequential . mapMonad f . runSequential

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

-- | Sequential importance sampling.
-- Applies a given transformation after each time step.
sis :: Monad m
    => (forall x. m x -> m x) -- ^ transformation
    -> Int -- ^ number of time steps
    -> Sequential m a
    -> m a
sis f k = finish . composeCopies k (advance . hoistFirst f)
