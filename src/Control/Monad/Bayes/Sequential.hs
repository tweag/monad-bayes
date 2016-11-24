{-|
Module      : Control.Monad.Bayes.Sequential
Description : Suspendable probabilistic computation
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts,
  RankNTypes,
  TypeFamilies
 #-}

module Control.Monad.Bayes.Sequential (
    Sequential,
    suspend,
    finish,
    advance,
    finished,
    hoistFirst,
    hoist
                ) where

import Control.Monad.Trans.Class
import Control.Monad.Coroutine hiding (suspend)
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Either

import Control.Monad.Bayes.Class

-- | Sequential represents a computation that can be paused at certain points.
-- The intermediate monadic effects can be extracted, which is particularly useful
-- for implementation of SMC-related methods.
-- All the probabilistic effects are lifted from the transformed monad,
-- but also `suspend` is inserted after each `factor`.
newtype Sequential m a = Sequential {runSequential :: (Coroutine (Await ()) m a)}
  deriving(Functor,Applicative,Monad,MonadTrans)
extract :: Await () a -> a
extract (Await f) = f ()

-- | A point where the computation is paused.
suspend :: Monad m => Sequential m ()
suspend = Sequential await

-- | Remove the remaining suspension points.
finish :: Monad m => Sequential m a -> m a
finish = pogoStick extract . runSequential

-- | Run to the next suspension point.
-- If the computation is finished do nothing.
--
-- > finish = finish . advance
advance :: Monad m => Sequential m a -> Sequential m a
advance = Sequential . bounce extract . runSequential

-- | Checks if no more suspension points remaining.
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

type instance CustomReal (Sequential m) = CustomReal m

instance MonadDist m => MonadDist (Sequential m) where
  primitive = lift . primitive

instance MonadBayes m => MonadBayes (Sequential m) where
  factor w = lift (factor w) >> suspend
