{-|
Module      : Control.Monad.Bayes.Inference
Description : Inference algorithms for probabilistic programs
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  ScopedTypeVariables
 #-}

module Control.Monad.Bayes.Inference (
  module Control.Monad.Bayes.Inference.MCMC,
  rejection,
  importance,
  importance',
  smcMultinomial,
  smcMultinomial',
  smcWithResampler,
  mh,
  mhInitPrior,
  mhPriorKernel,
  mhPrior,
  pimh,
  randomWalk,
  hmc,
  advi
) where

import Prelude hiding (sum)

import Numeric.AD.Internal.Reverse (Tape)
import Data.Reflection (Reifies)
import Control.Monad.RWS

import Numeric.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Rejection
import Control.Monad.Bayes.Sequential as Sequential
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Enumerator hiding (mass)
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Augmented
import Control.Monad.Bayes.Conditional
import Control.Monad.Bayes.Deterministic
import Control.Monad.Bayes.Constraint
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.MeanField
import Control.Monad.Bayes.Reparametrized
import Control.Monad.Bayes.Inference.MCMC hiding (mh)
import qualified Control.Monad.Bayes.Inference.MCMC as MCMC
import qualified Control.Monad.Bayes.Inference.Variational as VI

-- | Rejection sampling that proposes from the prior.
-- The accept/reject decision is made for the whole program rather than
-- individual variables.
-- The program must not contain factors larger than 1.
rejection :: Monad m => Int -- ^ number of samples accepted
                     -> Rejection m a -> m [a]
rejection n d = sequence $ replicate n s where
  s = do
    m <- runRejection d
    case m of Just x  -> return x
              Nothing -> s

-- | Simple importance sampling from the prior.
importance :: (Monad m, HasCustomReal m)
           => Int -- ^ numer of samples produced
           -> Population m a -> Population m a
importance n = (spawn n >>)

-- | Multiple importance samples with post-processing that aggregates weights of equal elements.
-- It does not normalize the weights.
importance' :: (Ord a, Monad m, HasCustomReal m) =>
               Int -> Population m a -> m [(a, CustomReal m)]
importance' n d = fmap compact $ explicitPopulation $ importance n d

-- | Sequential Monte Carlo from the prior with multinomial resampling.
smcMultinomial :: (Monad m, HasCustomReal m, NumSpec (CustomReal m), Sampleable (Discrete (CustomReal m) Int) m)
    => Int -- ^ number of resampling points
    -> Int -- ^ number of particles
    -> Sequential (Population m) a -> Population m a
smcMultinomial k n = smcWithResampler resample k n

-- | `smcMultinomial` with post-processing like in 'importance''.
smcMultinomial' :: (Ord a, Monad m, HasCustomReal m, NumSpec (CustomReal m), Sampleable (Discrete (CustomReal m) Int) m)
     => Int -> Int
     -> Sequential (Population m) a -> m [(a, CustomReal m)]
smcMultinomial' k n d = fmap compact $ explicitPopulation $ smcMultinomial k n d

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

-- | Like `smc`, but with a custom resampling scheme.
smcWithResampler :: (Monad m, HasCustomReal m) =>
  (forall x. Population m x -> Population m x) -- ^ resampling function
  -> Int -- ^ number of resampling points
  -> Int -- ^ number of particles
  -> Sequential (Population m) a -> Population m a

smcWithResampler resampler k n =
  finish . composeCopies k (advance . hoist' resampler) . hoist' (spawn n >>)
  where
    hoist' = Sequential.hoistFirst

-- | Obtain a valid trace by sampling from the prior.
-- To ensure that the trace has non-zero posterior density,
-- we additionally perform rejection sampling when the likelihood is zero.
traceFromPrior :: (HasCustomReal m, Monad m) => Augmented (Weighted m) a -> m (Trace (CustomReal m))
traceFromPrior model = do
  (t,w) <- runWeighted $ marginal $ joint model
  if w > 0 then
    return t
  else
    traceFromPrior model

-- | Metropolis-Hastings algorithm with a custom transition kernel operating on traces of programs.
mh :: (MonadDist m, MHKernel k, KernelDomain k ~ Trace (CustomReal m), MHSampler k ~ m)
         => Int -- ^ number of steps
         -> JointDensity (CustomReal m) a -- ^ model
         -> k -- ^ transition kernel
         -> Trace (CustomReal m) -- ^ starting trace not included in the output
         -> m [a] -- ^ resulting Markov chain truncated to output space
mh n model kernel start = fmap (map (unsafeDeterministic . prior . unsafeConditional model) . snd) $
  evalRWST (MCMC.mh n kernel) (unsafeJointDensity model) (start, unsafeJointDensity model start)

-- | Metropolis-Hastings that samples the initial state from the prior.
-- To ensure that the initial state has non-zero density, rejection sampling is used with rejection on zero likelihood.
mhInitPrior :: (MonadDist m, MHKernel k, KernelDomain k ~ Trace (CustomReal m), MHSampler k ~ m)
            => Int -- ^ number of steps
            -> (forall n. (MonadBayes n, CustomReal m ~ CustomReal n) => n a) -- ^ model
            -> k -- ^ transition kernel
            -> m [a] -- ^ resulting Markov chain truncated to output space
mhInitPrior n model kernel = traceFromPrior model >>= mh n model kernel

-- | Construct a Metropolis-Hastings kernel that ignores the input and samples a trace from the prior.
-- We need to constrain the output type of the program to '()' since otherwise GHC type inference fails.
mhPriorKernel :: MonadDist m
              => (forall n. (MonadDist n, CustomReal n ~ CustomReal m) => n ()) -- ^ model
              -> CustomKernel m (Trace (CustomReal m))
mhPriorKernel model = customKernel (const $ marginal $ joint model) (const $ unsafeJointDensity model)

-- | Metropolis-Hastings version that uses the prior as proposal distribution.
-- Current implementation is wasteful in that it computes the density of a trace twice.
-- This could be fixed by providing a specialized implementation instead.
mhPrior :: MonadDist m => Int -> (forall n. (MonadBayes n, CustomReal n ~ CustomReal m) => n a) -> m [a]
mhPrior n d = prior (marginal (joint d)) >>= mh n d (mhPriorKernel (prior d >> return ()))

-- | Sequential Independent Metropolis Hastings.
-- Outputs one sample per SMC run.
pimh :: MonadDist m => Int -- ^ number of resampling points in SMC
                    -> Int -- ^ number of particles in SMC
                    -> Int -- ^ number of independent SMC runs
                    -> (forall n. (MonadBayes n, CustomReal n ~ CustomReal m) => Sequential (Population n) a) -- ^ model
                    -> m [a]
pimh k np ns d = mhPrior ns $ collapse $ smcMultinomial k np d

-- | Random walk Metropolis-Hastings proposing single-site updates from a normal distribution with a fixed width.
randomWalk :: (MonadDist m)
    => Constraint (JointDensity (CustomReal m)) a -- ^ model
    -> CustomReal m -- ^ width of the Gaussian kernel @sigma@
    -> Int -- ^ number of transitions, equal to the number of samples returned
    -> Trace (CustomReal m) -- ^ starting trace not included in the output
    -> m [a]
randomWalk model sigma n start = mh n (unconstrain model) kernel start where
  kernel = randomWalkKernel sigma

-- | Hamitlonian Monte Carlo.
-- Only works for models with a fixed number of continuous random variables and no discrete random variables.
hmc :: (MonadDist m, CustomReal m ~ Double)
    => Constraint (JointDensityGradient (CustomReal m)) a -- ^ model
    -> CustomReal m -- ^ step size @epsilon@
    -> Int -- ^ number of steps @L@ taken at each transition
    -> CustomReal m -- ^ mass
    -> Int -- ^ number of transitions, equal to the number of samples returned
    -> Trace (CustomReal m) -- ^ starting trace not included in the output
    -> m [a]
hmc model epsilon l m n start =
  fmap (map (traceToOutput (unconstrain model)) . snd) $ -- map traces to outputs
    evalRWST (MCMC.mh n kernel) p (start, p start) where
      -- kernel only updates continouos variables
      kernel = traceKernel $ productKernel 1 (hamiltonianKernel (hmcParam epsilon l m) gradU) identityKernel
      -- to compute density first extract continous variables from the trace
      p = fst . pWithGrad . fst . toLists
      -- density gradient
      gradU = snd . pWithGrad
      pWithGrad = unsafeJointDensityGradient (unconstrain model)

-- | Automatic Differentiation Variational Inference.
-- Fits a mean field normal variational family in the unconstrained space using stochastic gradient descent.
advi :: forall m a. (MonadDist m)
     => Int -- ^ number of random variables in the model
     -> (forall n. (Monad n, Sampleable (Normal (CustomReal n)) n, Conditionable n) => Constraint (MeanFieldNormal n) a) -- ^ model
     -> CustomReal m -- ^ learning rate
     -> CustomReal m -- ^ decay rate
     -> Int -- ^ number of optimization steps
     -> m (m a) -- ^ optimized variational model
advi size model lr dr n = VI.advi size modelFirst modelSecond (VI.SGDParam lr dr n) where
  modelFirst :: (forall s. Reifies s Tape => MeanFieldNormal (Weighted (Reparametrized s m)) a)
  modelFirst = unconstrain model
  modelSecond :: (MeanFieldNormal (Weighted m) a)
  modelSecond = unconstrain model
