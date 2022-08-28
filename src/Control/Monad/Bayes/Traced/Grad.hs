{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Static
-- Description : Distributions on execution traces of full programs
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Grad
--   ( Traced,
--     hoist,
--     marginal,
--     mhStep,
--     mh,
--   )
where

import Control.Applicative (liftA2)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Traced.Common
  ( Trace (..),
    bind,
    mhTrans,
    scored,
    singleton,
  )
import Control.Monad.Bayes.Weighted (Weighted, weighted)
import Control.Monad.Trans (MonadTrans (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)
import Prelude hiding (Real)
import Control.Monad.State
import Control.Monad.Writer
import Numeric.Log (Log (ln, Exp))
import Numeric.AD
import Numeric.AD.Internal.Reverse (Reverse, Tape)
import Data.Reflection (Reifies)

newtype Density n a = Density (State [n] a) deriving newtype (Functor, Applicative, Monad)

instance MonadState [n] (Density n) where
  get = Density get
  put = Density . put


instance RealFloat n => MonadSample (Density n) where
  type (Real (Density n)) = n
  random = do
    trace <- get
    x <- case trace of
      [] -> error "ran out of randomness in the trace: this suggests that you are running HMC on a probabilistic program with stochastic control flow. Don't do that!"
      r : xs -> put xs >> pure r
    pure x

density :: Density n b -> [n] -> b
density (Density m) = evalState ( m)

tangent :: RealFloat n => (forall m . MonadInfer m => m a)  -> [n] -> [n]
tangent m = grad $ pdf m
    where 
        pdf m = ln . exp . snd . density (weighted m)

ex :: RealFloat n => [n] -> [n]
ex = tangent example

example :: MonadInfer m => m Bool
example = do
    x <- random
    factor (Exp $ log (x ** 2))
    return (x > 0.5)
