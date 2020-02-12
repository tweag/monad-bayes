{-|
Module      : Control.Monad.Bayes.Helpers
Description : Helper functions for working with inference monads
Copyright   : (c) Adam Scibior, 2015-2020
License     : MIT
Maintainer  : leonhard.markert@tweag.io
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Helpers (
  W,
  hoistW,
  P,
  hoistP,
  S,
  hoistS,
  F,
  hoistF,
  T,
  hoistT,
  hoistWF,
  hoistSP,
  hoistSTP
) where

import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Sequential as Seq
import Control.Monad.Bayes.Free as Free
import Control.Monad.Bayes.Traced as Tr

type W = Weighted
type P = Population
type S = Sequential
type F = FreeSampler
type T = Traced

hoistW :: (forall x. m x -> n x) -> W m a -> W n a
hoistW = Weighted.hoist

hoistP :: (Monad m, Monad n)
      => (forall x. m x -> n x) -> P m a -> P n a
hoistP = Pop.hoist

hoistS :: (forall x. m x -> m x) -> S m a -> S m a
hoistS = Seq.hoistFirst

hoistF :: (Monad m, Monad n) => (forall x. m x -> n x) -> F m a -> F n a
hoistF = Free.hoist


hoistWF :: (Monad m, Monad n)
      => (forall x. m x -> n x)
      -> W (F m) a -> W (F n) a
hoistWF m = hoistW $ hoistF m

hoistSP :: Monad m
        => (forall x. m x -> m x)
        -> S (P m) a -> S (P m) a
hoistSP m = hoistS $ hoistP m

hoistSTP :: Monad m
         => (forall x. m x -> m x)
         -> S (T (P m)) a -> S (T (P m)) a
hoistSTP m = hoistS $ hoistT $ hoistP m
