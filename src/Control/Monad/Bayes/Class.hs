{-|
Module      : Control.Monad.Bayes.Class
Description : Types for probabilistic modelling
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  GADTs
 #-}


module Control.Monad.Bayes.Class (
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
  MonadBayes,
  factor,
  condition,
  observe,
  CustomReal
) where

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Numeric.SpecFunctions
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS hiding (tell)
import Control.Monad.Trans.List
--import Control.Monad.Trans.Except
import Control.Monad.Trans.Cont

import Control.Monad.Bayes.Distribution
--import Control.Monad.Bayes.Primitive
import qualified Control.Monad.Bayes.LogDomain as Log

-- | Monads for building generative probabilistic models.
-- The class does not specify any conditioning primitives.
class (Monad m, Ord (CustomReal m), Log.NumSpec (CustomReal m), Real (CustomReal m),
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


-- | Probability monads that allow conditioning.
-- Both soft and hard conditions are allowed.
class MonadDist m => MonadBayes m where

    -- | Conditioning with an arbitrary factor, as found in factor graphs.
    -- Bear in mind that some inference algorithms may require `factor`s to be
    -- non-negative to work correctly.
    factor :: Log.LogDomain (CustomReal m) -> m ()

    -- | Hard conditioning on an arbitrary predicate.
    --
    -- > condition b = factor (if b then 1 else 0)
    condition :: Bool -> m ()
    condition b = factor $ if b then 1 else 0

    -- | Soft conditioning on a noisy value.
    --
    -- > observe d x = factor (pdf d x)
    observe :: (Density d, RealNumType d ~ CustomReal m) => d -> DomainType d -> m ()
    observe d x = factor (pdf d x)

----------------------------------------------------------------------------
-- Instances that lift probabilistic effects to standard tranformers.
type instance CustomReal (IdentityT m) = CustomReal m

instance (Sampleable d m, Monad m) => Sampleable d (IdentityT m) where
  sample = lift . sample

instance MonadDist m => MonadDist (IdentityT m)

instance MonadBayes m => MonadBayes (IdentityT m) where
  factor = lift . factor


type instance CustomReal (MaybeT m) = CustomReal m

instance (Sampleable d m, Monad m) => Sampleable d (MaybeT m) where
  sample = lift . sample

instance MonadDist m => MonadDist (MaybeT m)

instance MonadBayes m => MonadBayes (MaybeT m) where
  factor = lift . factor


type instance CustomReal (ReaderT r m) = CustomReal m

instance (Sampleable d m, Monad m) => Sampleable d (ReaderT r m) where
  sample = lift . sample

instance MonadDist m => MonadDist (ReaderT r m)

instance MonadBayes m => MonadBayes (ReaderT r m) where
  factor = lift . factor


type instance CustomReal (WriterT w m) = CustomReal m

instance (Sampleable d m, Monad m, Monoid w) => Sampleable d (WriterT w m) where
  sample = lift . sample

instance (Monoid w, MonadDist m) => MonadDist (WriterT w m)

instance (Monoid w, MonadBayes m) => MonadBayes (WriterT w m) where
  factor = lift . factor


type instance CustomReal (StateT s m) = CustomReal m

instance (Sampleable d m, Monad m) => Sampleable d (StateT s m) where
  sample = lift . sample

instance MonadDist m => MonadDist (StateT s m)

instance MonadBayes m => MonadBayes (StateT s m) where
  factor = lift . factor


type instance CustomReal (RWST r w s m) = CustomReal m

instance (Sampleable d m, Monad m, Monoid w) => Sampleable d (RWST r w s m) where
  sample = lift . sample

instance (Monoid w, MonadDist m) => MonadDist (RWST r w s m)

instance (Monoid w, MonadBayes m) => MonadBayes (RWST r w s m) where
  factor = lift . factor


type instance CustomReal (ListT m) = CustomReal m

instance (Sampleable d m, Monad m) => Sampleable d (ListT m) where
  sample = lift . sample

instance MonadDist m => MonadDist (ListT m)

instance MonadBayes m => MonadBayes (ListT m) where
  factor = lift . factor


-- ExceptT is commented out for compatibility with transformers <0.4
-- We could include it through transformers-compat if necessary
-- instance MonadDist m => MonadDist (ExceptT e m) where
--     primitive = lift . primitive
--
-- instance MonadDist m => MonadDist (ExceptT e m) where
--     factor = lift . factor


type instance CustomReal (ContT r m) = CustomReal m

instance (Sampleable d m, Monad m) => Sampleable d (ContT r m) where
  sample = lift . sample

instance MonadDist m => MonadDist (ContT r m)

instance MonadBayes m => MonadBayes (ContT r m) where
  factor = lift . factor
