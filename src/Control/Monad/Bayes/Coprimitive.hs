{-|
Module      : Control.Monad.Bayes.Coprimitive
Description : Coprimitive probability monad and density functions
Copyright   : (c) Yufei Cai, 2016
              (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  GADTs,
  StandaloneDeriving,
  TypeFamilies,
  DeriveFunctor,
  GeneralizedNewtypeDeriving,
  FlexibleContexts
   #-}

module Control.Monad.Bayes.Coprimitive (
  AwaitSampler (AwaitSampler),
  Coprimitive (Coprimitive),
  runCoprimitive,
  conditional,
  pseudoDensity,
  jointDensity,
  contJointDensity,
  unsafeContJointDensity
) where

import Control.Monad.Trans.Class
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Maybe

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Deterministic

-- | Suspension functor: yields primitive distribution, awaits sample.
data AwaitSampler r y where
  AwaitSampler :: Primitive r a -> (a -> y) -> AwaitSampler r y
deriving instance Functor (AwaitSampler r)

-- | Pause probabilistic program whenever a primitive distribution is
-- encountered, yield the encountered primitive distribution, and
-- await a sample of that primitive distribution.
newtype Coprimitive m a = Coprimitive
  { runCoprimitive :: Coroutine (AwaitSampler (CustomReal m)) m a
  }
  deriving (Functor, Applicative, Monad)

type instance CustomReal (Coprimitive m) = CustomReal m

instance MonadTrans Coprimitive where
  lift = Coprimitive . lift

instance (MonadDist m) => MonadDist (Coprimitive m) where
  primitive d = Coprimitive (suspend (AwaitSampler d return))

instance (MonadBayes m) => MonadBayes (Coprimitive m) where
  factor = lift . factor

-- | Conditional distribution given a subset of random variables.
-- For every fixed value its PDF is included as a `factor`.
-- Missing values and type mismatches are treated as no conditioning on that RV.
conditional :: MonadBayes m => Coprimitive m a -> [Maybe (Either Int (CustomReal m))] -> m a
conditional p xs = condRun (runCoprimitive p) xs where
  -- Draw a value from the prior and proceed.
  drawFromPrior :: MonadDist m => AwaitSampler (CustomReal m) a -> m a
  drawFromPrior (AwaitSampler d k) = fmap k (primitive d)

  condRun :: MonadBayes m => Coroutine (AwaitSampler (CustomReal m)) m a -> [Maybe (Either Int (CustomReal m))] -> m a
  -- No more variables to condition on - run from prior.
  condRun c [] = pogoStickM drawFromPrior c
  -- Condition on the next variable.
  condRun c (x:xs) = resume c >>= \t -> case t of
    -- Program finished - return the final result
    Right y -> return y
    -- Random draw encountered - conditional execution
    Left s@(AwaitSampler d@(Discrete   _) k) | Just (Left v) <- x ->
      -- A value is available and its type matches the current draw
      factor (pdf d v) >> condRun (k v) xs
    Left s@(AwaitSampler d@(Continuous _) k) | Just (Right v) <- x ->
      -- A value is available and its type matches the current draw
      factor (pdf d v) >> condRun (k v) xs
    Left s ->
      -- Value missing or type mismatch - ignore and draw from prior
      drawFromPrior s >>= (`condRun` xs)

-- | Evaluates joint density of a subset of random variables.
-- Specifically this is a product of conditional densities encountered in
-- the trace times the (unnormalized) likelihood of the trace.
-- Missing latent variables are integrated out using the transformed monad,
-- unused values from the list are ignored.
pseudoDensity :: MonadDist m => Coprimitive (Weighted m) a -> [Maybe (Either Int (CustomReal m))]
  -> m (LogDomain (CustomReal m))
pseudoDensity p xs = fmap snd $ runWeighted $ conditional p xs

-- | Joint density of all random variables in the program.
-- Failure occurs when the list is too short or when there's a type mismatch.
jointDensity :: MonadDist (Deterministic r) => Coprimitive (Weighted (Deterministic r)) a -> [Either Int r]
  -> Maybe (LogDomain r)
jointDensity p xs = maybeDeterministic $ pseudoDensity p (map Just xs)

-- | Like 'jointDensity', but assumes all random variables are continuous.
contJointDensity ::  MonadDist (Deterministic r) => Coprimitive (Weighted (Deterministic r)) a -> [r] -> Maybe (LogDomain r)
contJointDensity p xs = jointDensity p (map Right xs)

-- | Like 'contJointDensity', but throws an error if density can not be computed.
unsafeContJointDensity ::  MonadDist (Deterministic r) => Coprimitive (Weighted (Deterministic r)) a -> [r] -> LogDomain r
unsafeContJointDensity p xs = fromMaybe (error "Could not compute density: some random variables are discrete or the list of random variables is too short") $ contJointDensity p xs
