{-|
Module      : Control.Monad.Bayes.Inference.MCMC
Description : Markov chain Monte Carlo
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Bayes.Inference.MCMC (
  MHKernel,
  KernelDomain,
  MHSampler,
  proposeFrom,
  density,
  densityRatio,
  proposeWithDensityRatio,
  mhInitPrior,
  mh,
  mhStep,
  IdentityKernel,
  identityKernel,
  SymmetricKernel,
  trustSymmetricKernel,
  GaussianKernel,
  gaussianKernel,
  DiscreteKernel,
  discreteKernel,
  SingleSiteKernel,
  singleSiteKernel,
  ProductKernel,
  productKernel,
  TraceKernel,
  traceKernel,
  singleSiteTraceKernel,
  RandomWalkKernel,
  randomWalkKernel,
  CustomKernel,
  customKernel,
  HMCParam,
  hmcParam,
  stepSize,
  steps,
  mass,
  HamiltonianKernel,
  hamiltonianKernel
) where

import Prelude hiding (sum)

import Data.Bifunctor (first)
import Control.Monad.State

import Numeric.LogDomain
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Conditional
import Control.Monad.Bayes.Augmented

-- | Interface for transition kernels used in Metropolis-Hastings algorithms.
-- A kernel is a stochastic function @a -> D a@.
class (HasCustomReal (MHSampler k)) => MHKernel k where
  -- | Type of variables over which the kernel is defined.
  type KernelDomain k :: *
  -- | The monad in which the kernel can be sampled from.
  type MHSampler k :: * -> *
  -- | Propose a transition from the kernel starting at a given point.
  proposeFrom :: k -> KernelDomain k -> (MHSampler k) (KernelDomain k)
  -- | Probability density of making a particular transition.
  -- Bear in mind that it's not always obvious what base measure this density is taken w.r.t.
  density :: k -> KernelDomain k -> KernelDomain k -> LogDomain (CustomReal (MHSampler k))

  -- | Possibly optimized implementation of the ratio of reverse transition density and forward transition density.
  --
  -- > densityRatio k x y = density k y x / density k x y
  densityRatio :: k -> KernelDomain k -> KernelDomain k -> LogDomain (CustomReal (MHSampler k))
  densityRatio k x y = density k y x / density k x y

  -- | Possibly optimized implementation of the proposal combined with density ratio.
  --
  -- > proposeWithDensityRatio k x = fmap (\y -> (y, densityRatio k x y)) (proposeFrom k x)
  proposeWithDensityRatio :: Functor (MHSampler k) => k -> KernelDomain k -> (MHSampler k) (KernelDomain k, LogDomain (CustomReal (MHSampler k)))
  proposeWithDensityRatio k x = fmap (\y -> (y, densityRatio k x y)) (proposeFrom k x)

-- | Metropolis-Hastings that samples the initial state from the prior.
-- To ensure that the initial state has non-zero density, rejection sampling is used with rejection on zero likelihood.
mhInitPrior :: (MonadDist m, MHKernel k, KernelDomain k ~ Trace (CustomReal m), MHSampler k ~ m)
            => Int -- ^ number of steps
            -> (forall n. (MonadBayes n, CustomReal m ~ CustomReal n) => n a) -- ^ model
            -> k -- ^ transition kernel
            -> m [a] -- ^ resulting Markov chain truncated to output space
mhInitPrior n model kernel = starting >>= mh n model kernel where
  starting = do
    (x,w) <- runWeighted $ marginal $ joint model
    if w > 0 then
      return x
    else
      starting

-- | Metropolis-Hastings algorithm with a custom transition kernel operating on traces of programs.
mh :: (MonadDist m, MHKernel k, KernelDomain k ~ Trace (CustomReal m), MHSampler k ~ m)
         => Int -- ^ number of steps
         -> Conditional (Weighted m) a -- ^ model
         -> k -- ^ transition kernel
         -> Trace (CustomReal m) -- ^ starting trace not included in the output
         -> m [a] -- ^ resulting Markov chain truncated to output space
mh n model kernel starting = evalStateT (sequence $ replicate n $ mhStep model kernel) starting where

-- | One step of 'mh'.
mhStep :: (MonadDist m, MHKernel k, KernelDomain k ~ Trace (CustomReal m), MHSampler k ~ m)
             => Conditional (Weighted m) a -- ^ model
             -> k -- ^ transition kernel
             -> StateT (Trace (CustomReal m)) m a -- ^ state carries the current trace
mhStep model kernel = do
  t <- get
  (t',r) <- lift $ proposeWithDensityRatio kernel t
  p <- lift $ unsafeDensity model t
  p' <- lift $ unsafeDensity model t'
  let ratio = min 1 (r * p' / p)
  accept <- bernoulli (fromLogDomain ratio)
  let tnew = if accept then t' else t
  put tnew
  (output,_) <- lift $ runWeighted $ unsafeConditional model tnew
  return output


-------------------------------------------------
-- Concrete transition kernels

-- | Identity kernel proposing the supplied value.
-- Equivalently a Dirac delta distribution on the argument.
-- Evaluating 'density' or 'densityRatio' for 'IdentityKernel' is an error since we prefer not to require equality
-- instance for the domain type.
-- However, 'proposeWithDensityRatio' returns the ratio of 1 for convenience.
data IdentityKernel (m :: * -> *) a = IdentityKernel

instance (HasCustomReal m, Applicative m) => MHKernel (IdentityKernel m a) where
  type KernelDomain (IdentityKernel m a) = a
  type MHSampler (IdentityKernel m a) = m
  proposeFrom _ x = pure x
  density _ _ _ = error "Identity kernel does not have density"
  densityRatio _ _ _ = error "Identity kernel does not have density ratio"
  proposeWithDensityRatio _ x = pure (x,1)

identityKernel :: (HasCustomReal m, Applicative m) => IdentityKernel m a
identityKernel = IdentityKernel


-- | A wrapper for kernels known to be symmetric, used to avoid unnecessarily computing density ratio which is always 1.
newtype SymmetricKernel k = SymmetricKernel k

instance MHKernel k => MHKernel (SymmetricKernel k) where
  type KernelDomain (SymmetricKernel k) = KernelDomain k
  type MHSampler (SymmetricKernel k) = MHSampler k
  proposeFrom (SymmetricKernel k) x = proposeFrom k x
  density (SymmetricKernel k) x y = density k x y
  densityRatio _ _ _ = 1

-- | At your own risk assert that a kernel is symmetric.
-- 'densityRatio' will always return 1, as will 'proposeWithDensityRatio'.
trustSymmetricKernel :: MHKernel k
                     => k -- ^ kernel known to be symmetric
                     -> SymmetricKernel k
trustSymmetricKernel = SymmetricKernel


-- | A transition kernel using a Gaussian distribution centered on the current value.
newtype GaussianKernel m = GaussianKernel (CustomReal m)

instance (HasCustomReal m, Sampleable (Normal (CustomReal m)) m) => MHKernel (GaussianKernel m) where
  type KernelDomain (GaussianKernel m) = CustomReal m
  type MHSampler (GaussianKernel m) = m
  proposeFrom (GaussianKernel sigma) x = normal x sigma
  density (GaussianKernel sigma) x y = pdf (normalDist x sigma) y
  densityRatio _ _ _ = 1

-- | Construct a Gaussian kernel with specified width.
gaussianKernel :: (HasCustomReal m, Sampleable (Normal (CustomReal m)) m)
               => CustomReal m -- ^ standard deviation of the Gaussian distribution, must be positive
               -> GaussianKernel m
gaussianKernel sigma | sigma <= 0 = error "Gaussian kernel: supplied width was not positive"
gaussianKernel sigma = GaussianKernel sigma


-- | A kernel on a list of variables that applies the supplied kernel to one of the variables randomly with
-- uniform probability.
-- It never changes the length of the list and computing density for a transition that changes the lenght is an error.
-- Density for a transition that changes multiple variables simultaneously is zero.
newtype SingleSiteKernel k = SingleSiteKernel k

instance (MHKernel k, Eq (KernelDomain k), MonadDist (MHSampler k)) => MHKernel (SingleSiteKernel k) where
  type KernelDomain (SingleSiteKernel k) = [KernelDomain k]
  type MHSampler (SingleSiteKernel k) = MHSampler k
  proposeFrom (SingleSiteKernel k) xs = if null xs then return [] else do
    index <- uniformD [0 .. length xs - 1]
    let (a, x:b) = splitAt index xs
    x' <- proposeFrom k x
    return (a ++ (x':b))
  density (SingleSiteKernel k) xs ys | (length xs == length ys) =
    let
      n = length xs
      diffs = filter (uncurry (/=)) $ zip xs ys
    in
      if n == 0 then 1 else
        case length diffs of
          0 -> sum (zipWith (density k) xs ys) / fromIntegral n
          1 -> let [(x,y)] = diffs in density k x y / fromIntegral n
          _ -> 0
  density _ xs ys =
    error $ "Single site kernel density: given lists have different lengths " ++ show (length xs) ++
            " and " ++ show (length ys)
  densityRatio (SingleSiteKernel k) xs ys | (length xs == length ys) =
    let
      n = length xs
      diffs = filter (uncurry (/=)) $ zip xs ys
    in
      if n == 0 then 1 else
        case length diffs of
          0 -> sum (zipWith (density k) ys xs) / sum (zipWith (density k) xs ys)
          1 -> let [(x,y)] = diffs in densityRatio k x y
          _ -> 0 / 0
  densityRatio _ xs ys =
    error $ "Single site kernel density ratio: given lists have different lengths " ++ show (length xs) ++
            " and " ++ show (length ys)
  -- custom proposeWithDensityRatio function could forgo length checking in densityRatio

-- | Construct a single site version of a supplied kernel.
singleSiteKernel :: (MHKernel k, MonadDist (MHSampler k))
                 => k -- ^ kernel to be applied to one variable
                 -> SingleSiteKernel k
singleSiteKernel = SingleSiteKernel


-- | Product kernel updating one of the two variables with specified probability.
data ProductKernel k l = ProductKernel (CustomReal (MHSampler k)) k l

instance (MHKernel k, MHKernel l, MHSampler k ~ MHSampler l, MonadDist (MHSampler k), Eq (KernelDomain k), Eq (KernelDomain l)) => MHKernel (ProductKernel k l) where
  type KernelDomain (ProductKernel k l) = (KernelDomain k, KernelDomain l)
  type MHSampler (ProductKernel k l) = MHSampler k
  proposeFrom (ProductKernel ratio k l) (x,y) = do
    useFirst <- bernoulli ratio
    if useFirst then do
      x' <- proposeFrom k x
      return (x', y)
    else do
      y' <- proposeFrom l y
      return (x, y')
  density (ProductKernel ratio k l) (x,y) (x',y') = let p = toLogDomain ratio in
    case (x == x', y == y') of
      (True, True)  -> p * (density k x x') + (1-p) * (density l y y')
      (False, True) -> p * density k x x'
      (True, False) -> (1-p) * density l y y'
      (False, False) -> 0
  densityRatio (ProductKernel ratio k l) (x,y) (x',y') = let p = toLogDomain ratio in
    case (x == x', y == y') of
      (True, True)  -> (p * (density k x' x) + (1-p) * (density l y' y)) /
                       (p * (density k x x') + (1-p) * (density l y y'))
      (False, True) -> densityRatio k x x'
      (True, False) -> densityRatio l y y'
      (False, False) -> 0 / 0

-- | Construct a product kernel that randomly proposes a change for one of the variables.
productKernel :: (MHKernel k, MHKernel l, MHSampler k ~ MHSampler l, MonadDist (MHSampler k),
                  Eq (KernelDomain k), Eq (KernelDomain l))
              => CustomReal (MHSampler k) -- ^ probability of proposing a change to the first variable
              -> k -- ^ kernel to be used on the first variable
              -> l -- ^ kernel to be used on the second variable
              -> ProductKernel k l
productKernel ratio k l =
  if ratio >= 0 && ratio <= 1 then
    ProductKernel ratio k l
  else
    error $ "Product kernel was given invalid ratio"


-- | Discrete kernel proposing transitions on the set [0..n-1]
newtype DiscreteKernel m = DiscreteKernel [[CustomReal m]]

checkRange :: DiscreteKernel m -> Int -> a -> a
checkRange (DiscreteKernel t) x a =
  if x >= 0 && x < length t then
    a
  else
    error $ "Discrete kernel: was given value " ++ (show x) ++ " which is out of range 0.." ++ show (length t)

instance (HasCustomReal m, NumSpec (CustomReal m), Sampleable (Discrete (CustomReal m) Int) m) => MHKernel (DiscreteKernel m) where
  type KernelDomain (DiscreteKernel m) = Int
  type MHSampler (DiscreteKernel m) = m
  proposeFrom k@(DiscreteKernel t) x = checkRange k x `seq` discrete (t !! x)
  density k@(DiscreteKernel t) x y = checkRange k x `seq` checkRange k y `seq` toLogDomain (t !! x !! y)

-- | Construct a discrete kernel using the specified transition matrix.
discreteKernel :: (HasCustomReal m, NumSpec (CustomReal m), Sampleable (Discrete (CustomReal m) Int) m)
               => [[CustomReal m]] -- ^ matrix of transition probabilities - outer list indexes inputs, inner outputs
                                   -- must have shape NxN, the rows are automatically normalized
               -> DiscreteKernel m
discreteKernel t = DiscreteKernel (normalizedT) where
  normalizedT = checkShape `seq` map normalize t
  checkShape =
    if all (\x -> length x == length t) t then
      ()
    else
      error $ "Discrete kernel: invalid transition matrix shape " ++ (show (map length t))


-- | Wrapper for kernels that operate on traces of proabilistic programs.
newtype TraceKernel k = TraceKernel k

instance (MHKernel k, KernelDomain k ~ ([CustomReal (MHSampler k)], [Int]), Functor (MHSampler k))
         => MHKernel (TraceKernel k) where
  type KernelDomain (TraceKernel k) = Trace (CustomReal (MHSampler k))
  type MHSampler (TraceKernel k) = MHSampler k
  proposeFrom (TraceKernel k) x = fmap fromLists $ proposeFrom k (toLists x)
  density (TraceKernel k) x y = density k (toLists x) (toLists y)
  densityRatio (TraceKernel k) x y = densityRatio k (toLists x) (toLists y)
  proposeWithDensityRatio (TraceKernel k) x =
    fmap (first fromLists) $ proposeWithDensityRatio k (toLists x)

-- | Assert that a given kernel acts on traces of probabilistic programs.
traceKernel :: (MHKernel k, KernelDomain k ~ ([CustomReal (MHSampler k)], [Int]), Functor (MHSampler k))
            => k -- ^ kernel operating on lists of random variables
            -> TraceKernel k
traceKernel = TraceKernel

-- | Construct a kernel on traces from single site kernels.
singleSiteTraceKernel :: (MHKernel k, MHKernel l, KernelDomain k ~ CustomReal (MHSampler k), KernelDomain l ~ Int,
                          MHSampler k ~ MHSampler l, MonadDist (MHSampler k), Eq (KernelDomain k))
                      => CustomReal (MHSampler k) -- ^ probability of proposing a change to a continuous variable
                      -> k -- ^ kernel to be used on a continuous variable
                      -> l -- ^ kernel to be used on a discrete variable
                      -> TraceKernel (ProductKernel (SingleSiteKernel k) (SingleSiteKernel l))
singleSiteTraceKernel ratio k l = TraceKernel $ productKernel ratio (singleSiteKernel k) (singleSiteKernel l)

type RandomWalkKernel m = TraceKernel (ProductKernel (SingleSiteKernel (GaussianKernel m))
                                                     (SingleSiteKernel (IdentityKernel m Int)))

-- | Random walk kernel updating only continuous variables.
randomWalkKernel :: (MonadDist m)
                 => CustomReal m -- ^ Width (standard deviation) of the Gaussian distribution.
                 -> RandomWalkKernel m
randomWalkKernel sigma = singleSiteTraceKernel 1 (gaussianKernel sigma) identityKernel


-- | A custom transition kernel specified as a pair of functions.
data CustomKernel m a = CustomKernel {customProposal :: a -> m a, customDensity :: a -> a -> LogDomain (CustomReal m)}

instance HasCustomReal m => MHKernel (CustomKernel m a) where
  type KernelDomain (CustomKernel m a) = a
  type MHSampler (CustomKernel m a) = m
  proposeFrom k x = customProposal k x
  density k x y = customDensity k x y

-- | Construct a custom kernel using the supplied sampler and its density.
-- It is up to the user to ensure that the density matches the sampler and that it does not return spurious values.
customKernel :: (a -> m a) -> (a -> a -> LogDomain (CustomReal m)) -> CustomKernel m a
customKernel = CustomKernel

data HMCParam r = HMCParam {stepSize :: r, steps :: Int, mass :: r}

-- | Kernel proposing new values for a collection of random variables using Hamiltonian dynamics.
-- Momenta are sampled from a normal distribution at each transition.
-- Even though Hamiltonian dynamics preserve the Hamiltonian, discretization of those differential equations
-- introduces errors that need to be corrected using the Metropolis-Hastings acceptance step.
-- Since even discretized dynamics are symmetric, 'proposeWithDensityRatio' always returns 1 as the second output.
-- However, density with respect to Lebesgue measure does not exist
-- and evaluating 'density' or 'densityRatio' is an error.
-- There exists a density w.r.t. the counting measure on reals but that is probably not very useful
-- and potentially confusing to implement.
data HamiltonianKernel m =
  HamiltonianKernel (HMCParam (CustomReal m)) ([CustomReal m] -> [CustomReal m])

instance (HasCustomReal m, Sampleable (Normal (CustomReal m)) m, Monad m) => MHKernel (HamiltonianKernel m) where
  type KernelDomain (HamiltonianKernel m) = [CustomReal m]
  type MHSampler (HamiltonianKernel m) = m
  proposeFrom (HamiltonianKernel (HMCParam e l m) dU) xs = do
    ps <- sequence $ replicate (length xs) (normal 0 (sqrt (recip m)))
    let (ys,_) = leapfrog l e dU m xs ps
    return ys
  density _ _ _ = error $ "Hamiltonian kernel does not have density"
  densityRatio _ _ _ = error $ "Hamiltonian kernel does not have density ratio"
  proposeWithDensityRatio k xs = fmap (,1) (proposeFrom k xs)

-- | Construct a Hamiltonian transition kernel.
hamiltonianKernel :: (HasCustomReal m, Sampleable (Normal (CustomReal m)) m, Monad m)
                  => HMCParam (CustomReal m)
                  -> ([CustomReal m] -> [CustomReal m]) -- gradient of the potential function w.r.t. positions
                  -> HamiltonianKernel m
hamiltonianKernel = HamiltonianKernel

-- | Package HMC parameters validating their values.
hmcParam :: (Ord r, Floating r)
         => r -- ^ step size @epsilon@ for discretizing Hamilton equations
         -> Int -- ^ number of discrete steps @L@ taken at each transition
         -> r -- ^ mass multiplying the identity mass matrix @M@
         -> HMCParam r
hmcParam e l m =
  checkE `seq` checkL `seq` checkM `seq` HMCParam e l m where
    checkE =
      if e <= 0 then
        error $ "Hamiltonian kernel: step size was not positive"
      else
        ()
    checkL =
      if l <= 0 then
        error $ "Hamiltonian kernel: number of steps was not positive"
      else
        ()
    checkM =
      if m <= 0 then
        error $ "Hamiltonian kernel: mass was not positive"
      else
        ()

-- | Leapfrog integration of Hamilton equations.
leapfrog :: Fractional r => Int -> r -> ([r] -> [r]) -> r -> [r] -> [r] -> ([r],[r])
leapfrog l e dU m qs ps = applyN l (uncurry (leapfrogStep e dU m)) (qs,ps) where
  applyN 0 _ x = x
  applyN n f x = applyN (n-1) f (f x)

-- | One step of 'leapfrog'.
leapfrogStep :: Fractional r => r -> ([r] -> [r]) -> r -> [r] -> [r] -> ([r],[r])
leapfrogStep e dU m qs ps = moveP $ moveQ $ moveP (qs,ps) where
  moveP (xs,vs) = (xs, zipWith (-) vs (map (* (e/2)) (dU xs)))
  moveQ (xs,vs) = (zipWith (+) xs (map (\v -> v * e / m) vs), vs)
