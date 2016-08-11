{-# LANGUAGE
  GeneralizedNewtypeDeriving
 #-}

module Control.Monad.Bayes.Deterministic(
  Deterministic,
  maybeDeterministic
) where

import Control.Monad.Bayes.Class

-- | A wrapper for deterministic code.
-- If a program doesn't actually use probabilistic effects,
-- but the type system can not ensure that,
-- `Deterministic` can be used to remove the `MonadBayes` constraint
-- in a type-safe way.
newtype Deterministic a = Deterministic (Maybe a)
  deriving(Functor, Applicative, Monad)

instance MonadDist Deterministic where
  primitive d = Deterministic Nothing

instance MonadBayes Deterministic where
  factor w = Deterministic Nothing

-- | Converts a probabilistic type into a deterministic one,
-- provided that no probabilistic effects are actually used.
maybeDeterministic :: Deterministic a -> Maybe a
maybeDeterministic (Deterministic m) = m
