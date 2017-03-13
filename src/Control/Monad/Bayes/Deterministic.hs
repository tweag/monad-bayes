{-|
Module      : Control.Monad.Bayes.Deterministic
Description : Escaping probabilistic computation that is in fact deterministic
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Deterministic(
  Deterministic,
  maybeDeterministic,
  unsafeDeterministic
) where

import Data.Maybe (fromMaybe)

import Control.Monad.Bayes.LogDomain (NumSpec)
import Control.Monad.Bayes.Class

-- | A wrapper for deterministic code.
-- If a program doesn't actually use probabilistic effects,
-- but the type system can not ensure that,
-- `Deterministic` can be used to remove the `MonadBayes` constraint
-- in a type-safe way.
--
-- The first parameter is a numeric type used to represent real numbers.
newtype Deterministic r a = Deterministic (Maybe a)
  deriving(Functor, Applicative, Monad)

type instance CustomReal (Deterministic r) = r

instance Sampleable d (Deterministic r) where
  sample _ = Deterministic Nothing

instance (Ord r, Real r, NumSpec r) => MonadDist (Deterministic r)

instance (Ord r, Real r, NumSpec r) => MonadBayes (Deterministic r) where
  factor _ = Deterministic Nothing

-- | Converts a probabilistic type into a deterministic one,
-- provided that no probabilistic effects are actually used.
maybeDeterministic :: Deterministic r a -> Maybe a
maybeDeterministic (Deterministic m) = m

-- | Converts a probabilistic type into a deterministic one,
-- throws an error if probabilistic effects were actually used.
unsafeDeterministic :: Deterministic r a -> a
unsafeDeterministic = fromMaybe (error "Deterministic: There were probabilistic effects") . maybeDeterministic
