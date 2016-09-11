{-# LANGUAGE
  RankNTypes,
  TypeFamilies
 #-}

module Control.Monad.Bayes (
  module Control.Monad.Bayes.Class,
  module Control.Monad.Bayes.Sampler,
  Model,
  Sample,
  enumerate,
  rejection,
  importance,
  smc,
  smcrm,
  traceMH,
  pimh
) where

import Control.Arrow

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Empirical
import Control.Monad.Bayes.Sampler
import qualified Control.Monad.Bayes.Dist as Dist
import qualified Control.Monad.Bayes.Inference as Infer

type Model a = forall m. (MonadBayes m, CustomReal m ~ Double) => m a
type Sample a = forall m. (MonadDist m, CustomReal m ~ Double) => m a

processPopulation :: MonadDist m => Population m a -> m [(a, CustomReal m)]
processPopulation = fmap (map (second fromLogDomain)) . runPopulation

enumerate :: Ord a => Model a -> [(a,Double)]
enumerate = Dist.enumerate

rejection :: Int -> Model a -> Sample [a]
rejection n d = Infer.rejection n d

importance :: Int -> Model a -> Sample [(a,Double)]
importance = Infer.importance

smc :: Int -> Int -> Model a -> Sample [(a,Double)]
smc k n d = processPopulation $ Infer.smc k n d

smcrm :: Int -> Int -> Int -> Model a -> Sample [(a,Double)]
smcrm k s n d = processPopulation $ Infer.smcrm k s n d

traceMH :: Int -> Model a -> Sample [a]
traceMH = Infer.traceMH

pimh :: Int -> Int -> Int -> Model a -> Sample [a]
pimh = Infer.pimh
