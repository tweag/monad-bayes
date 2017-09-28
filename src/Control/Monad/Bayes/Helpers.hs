{-|
Module      : Control.Monad.Bayes.Helpers
Description : Helper functions for working with inference monads
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
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
  hoistM,
  hoistT,
  hoistPM,
  hoistSP,
  hoistSPM
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

hoistF :: (Monad m) => (forall x. m x -> n x) -> F m a -> F n a
hoistF = Free.hoist


hoistPM :: Monad m
      => (forall x. m x -> m x)
      -> P (T m) a -> P (T m) a
hoistPM m = hoistP $ hoistM m

hoistSP :: Monad m
        => (forall x. m x -> m x)
        -> S (P m) a -> S (P m) a
hoistSP m = hoistS $ hoistP m

hoistSPM :: Monad m
         => (forall x. m x -> m x)
         -> S (P (T m)) a -> S (P (T m)) a
hoistSPM m = hoistS $ hoistPM m
