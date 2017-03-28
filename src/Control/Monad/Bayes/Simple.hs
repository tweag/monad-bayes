{-|
Module      : Control.Monad.Bayes.Simple
Description : Simplified type classes for probabilistic programming
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  GADTs
 #-}

 module Control.Monad.Bayes.Simple (
   module Control.Monad.Bayes.Distribution,
   MonadDist,
   categorical,
   logCategorical,
   logDiscrete,
   bernoulli,
   binomial,
   multinomial,
   geometric,
   poisson,
   uniformD,
   exponential,
   dirichlet,
   MonadBayes
 ) where

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Numeric.SpecFunctions
import Control.Arrow (first)

import Control.Monad
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS hiding (tell)
import Control.Monad.Trans.List
import Control.Monad.Trans.Cont

import qualified Numeric.LogDomain as Log
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Distribution

-- | Monads for building generative probabilistic models.
-- The class does not specify any conditioning primitives.
class (Monad m, HasCustomReal m, Log.NumSpec (CustomReal m), Real (CustomReal m),
       Sampleable (Normal (CustomReal m)) m, Sampleable (Gamma (CustomReal m)) m,
       Sampleable (Beta (CustomReal m)) m, Sampleable (Uniform (CustomReal m)) m,
       Sampleable (Discrete (CustomReal m) Int) m) => MonadDist m where

    -- | Categorical distribution.
    -- The weights should conform to the same rules as for `discrete`.
    --
    -- > discrete xs = categorical (zip [0..] xs)
    categorical :: [(a,CustomReal m)] -> m a
    categorical d = do
      i <- discrete (map snd d)
      return (fst (d !! i))

    -- | Like 'categorical', but weights are given in log domain.
    logCategorical :: [(a, Log.LogDomain (CustomReal m))] -> m a
    logCategorical d = do
      i <- logDiscrete (map snd d)
      return (fst (d !! i))

    -- | Like 'discrete', but weights are given in log domain.
    logDiscrete :: [Log.LogDomain (CustomReal m)] -> m Int
    logDiscrete ps = discrete $ fmap (Log.fromLogDomain . ( / Fold.maximum ps)) ps

    -- | Bernoulli distribution.
    --
    -- > bernoulli p = categorical [(True,p), (False,1-p)]
    bernoulli :: CustomReal m -> m Bool
    bernoulli p | p >= 0 && p <= 1 = categorical [(True,p), (False,1-p)]
    bernoulli p = error $ "Bernoulli: argument " ++ show (realToFrac p :: Double) ++ " is out of range [0,1]."

    -- | Binomial distribution. Returns the number of successes.
    binomial :: Int -> CustomReal m -> m Int
    binomial n _ | n < 0 = error $ "Binomial: the number of trials " ++ show n ++ " is negative"
    binomial _ p | p < 0 || p > 1 = error $ "Binomial: argument " ++ show (realToFrac p :: Double) ++ " is out of range [0,1]."
    binomial n p = categorical $ map (\k -> (k, mass k)) [0..n] where
                     mass k = realToFrac (n `choose` k) * (p ^ k) *
                              ((1-p) ^ (n-k))

    -- | Multinomial distribution.
    -- Corresponds to multiple independent draws from `categorical`.
    -- `multinomial` is to `categorical` as `binomial` is to `bernoulli`.
    multinomial :: [(a,CustomReal m)] -> Int -> m [(a,Int)]
    multinomial ps n = do
      let (xs,ws) = unzip ps
      indexes <- sequence $ replicate n $ discrete ws
      let counts = Map.toList $ Map.fromListWith (+) (zip indexes (repeat 1))
      return $ map (first (xs !!)) counts

    -- | Geometric distribution starting at 0.
    geometric :: CustomReal m -> m Int
    geometric p | p <= 0 || p > 1 = error $ "Geometric: argument " ++ show (realToFrac p :: Double) ++ " is out of range [0,1]."
    geometric p = discrete $ map ((p *) . (q ^)) ([0..] :: [Int]) where
                             q = 1 - p

    -- | Poisson distribution.
    poisson :: CustomReal m -> m Int
    poisson p | p <= 0 = error $ "Poisson: argument " ++ show (realToFrac p :: Double) ++ " is not positive."
    poisson p = discrete $ map mass [0..] where
                             mass k = c * (p ^ k) /
                                realToFrac (factorial k)
                             c = exp (-p)

    -- | Uniform discrete distribution.
    -- The list should be non-empty and finite.
    uniformD :: [a] -> m a
    uniformD xs | length xs == 0 = error $ "UniformD: the argument list is empty"
    uniformD xs = categorical $ map (,weight) xs where
                             weight = 1 / fromIntegral (length xs)

    -- | Exponential distribution parameterized by rate.
    --
    -- > exponential r = gamma 1 (1/r)
    exponential :: CustomReal m -> m (CustomReal m)
    exponential rate = gamma 1 (1 / rate)

    -- | Dirichlet distribution, the conjugate prior to `categorical`.
    -- The list should be finite and its elements should be positive.
    dirichlet :: [CustomReal m] -> m [CustomReal m]
    dirichlet ws = liftM normalize $ gammas ws where
      gammas = mapM (\w -> gamma w 1)
      normalize xs = map (/ (Prelude.sum xs)) xs


-- | Monads for building probabilistic programs with conditioning.
class (MonadDist m, Conditionable m) => MonadBayes m



-------------------------------------------------
-- Instances for standard transformers.

instance MonadDist m => MonadDist (IdentityT m)
instance MonadBayes m => MonadBayes (IdentityT m)

instance MonadDist m => MonadDist (MaybeT m)
instance MonadBayes m => MonadBayes (MaybeT m)

instance MonadDist m => MonadDist (ReaderT r m)
instance MonadBayes m => MonadBayes (ReaderT r m)

instance (Monoid w, MonadDist m) => MonadDist (WriterT w m)
instance (Monoid w, MonadBayes m) => MonadBayes (WriterT w m)

instance MonadDist m => MonadDist (StateT s m)
instance MonadBayes m => MonadBayes (StateT s m)

instance (Monoid w, MonadDist m) => MonadDist (RWST r w s m)
instance (Monoid w, MonadBayes m) => MonadBayes (RWST r w s m)

instance MonadDist m => MonadDist (ListT m)
instance MonadBayes m => MonadBayes (ListT m)

instance MonadDist m => MonadDist (ContT r m)
instance MonadBayes m => MonadBayes (ContT r m)
