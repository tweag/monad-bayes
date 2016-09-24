{-|
Module      : Control.Monad.Bayes.Conditional
Description : Distributions conditional on some of the variables
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  TypeFamilies,
  FlexibleContexts
   #-}

module Control.Monad.Bayes.Conditional where

import Control.Monad.State
import Data.Maybe

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Deterministic

newtype Conditional m a = Conditional (StateT ([Maybe (CustomReal m)], [Maybe Int]) m a)
  deriving (Functor, Applicative, Monad)

type instance CustomReal (Conditional m) = CustomReal m

instance MonadTrans Conditional where
  lift m = Conditional (lift m)

instance MonadBayes m => MonadDist (Conditional m) where
  primitive d = Conditional $ do
    (xs, cs) <- get
    case d of
      Discrete _ | (Just c):cs' <- cs -> do
        factor (pdf d c)
        put (xs, cs')
        return c
      Discrete _ | Nothing:cs' <- cs -> do
        put (xs, cs')
        primitive d
      Continuous _ | (Just x):xs' <- xs -> do
        factor (pdf d x)
        put (xs', cs)
        return x
      Continuous _ | Nothing:xs' <- xs -> do
        put (xs', cs)
        primitive d
      _ -> primitive d

instance MonadBayes m => MonadBayes (Conditional m) where
  factor = lift . factor

conditional :: Monad m => Conditional m a -> ([Maybe (CustomReal m)], [Maybe Int]) -> m a
conditional (Conditional m) = evalStateT m

pseudoDensity :: MonadDist m => Conditional (Weighted m) a -> ([Maybe (CustomReal m)], [Maybe Int]) -> m (LogDomain (CustomReal m))
pseudoDensity m = fmap snd . runWeighted . conditional m

jointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> ([r], [Int]) -> Maybe (LogDomain r)
jointDensity m (xs,cs) = maybeDeterministic $ pseudoDensity m (map Just xs, map Just cs)

contJointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> [r] -> Maybe (LogDomain r)
contJointDensity m xs = jointDensity m (xs,[])

unsafeContJointDensity :: MonadDist (Deterministic r) => Conditional (Weighted (Deterministic r)) a -> [r] -> LogDomain r
unsafeContJointDensity m xs = fromMaybe (error "Could not compute density: some random variables are discrete or the list of random variables is too short") $ contJointDensity m xs
