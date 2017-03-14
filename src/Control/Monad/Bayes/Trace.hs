{-|
Module      : Control.Monad.Bayes.Trace
Description : Probabilistic computation accumulating a trace
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  GADTs,
  DeriveFunctor,
  ScopedTypeVariables
   #-}

module Control.Monad.Bayes.Trace (
  Trace,
  fromLists,
  toLists,
) where

-- | Trace of a probabilistic program is a collection of values for all the
-- latent random variables.
newtype Trace r = Trace ([r],[Int])
  deriving(Monoid)

-- | Package values as a trace.
fromLists :: ([r],[Int]) -> Trace r
fromLists = Trace

-- | Extract values from a trace.
toLists :: Trace r -> ([r],[Int])
toLists (Trace t) = t
