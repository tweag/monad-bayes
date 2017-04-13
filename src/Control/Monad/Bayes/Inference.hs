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
  rejection,
  importance,
  importance',
  smcMultinomial,
  smcMultinomial',
  smcWithResampler,
  mhPrior,
  pimh,
  Kernel (Kernel),
  proposal,
  density,
  mhCustom,
  mhCustomStep,
  gaussianKernel,
  customDiscreteKernel,
  singleSiteTraceKernel,
  randomWalkKernel
) where

import Prelude hiding (sum)
import Control.Monad.State.Lazy

import Numeric.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Rejection
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sequential as Sequential
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Conditional

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



-- | Metropolis-Hastings kernel. Generates a new value and the MH ratio.
newtype MHKernel m a = MHKernel {runMHKernel :: a -> m (a, LogDomain (CustomReal m))}

-- | Generic Metropolis-Hastings algorithm.
mh :: MonadDist m => Int ->  Weighted m a -> MHKernel (Weighted m) a -> m [a]
mh n initial trans = evalStateT (start >>= chain n) 1 where
  -- start :: StateT LogFloat m a
  start = do
    (x, p) <- lift $ runWeighted initial
    if p == 0 then
      start
    else
      put p >> return x

  --chain :: Int -> a -> StateT LogFloat m [a]
  chain 0 _ = return []
  chain k x = do
    p <- get
    ((y,w), q) <- lift $ runWeighted $ runMHKernel trans x
    accept <- bernoulli $ if p == 0 then 1 else min 1 $ fromLogDomain (q * w / p)
    let next = if accept then y else x
    when accept (put q)
    rest <- chain (k-1) next
    return (x:rest)

-- | Metropolis-Hastings version that uses the prior as proposal distribution.
mhPrior :: MonadDist m => Int -> Weighted m a -> m [a]
mhPrior n d = mh n d kernel where
    kernel = MHKernel $ const $ fmap (,1) d

-- | Sequential Independent Metropolis Hastings.
-- Outputs one sample per SMC run.
pimh :: MonadDist m => Int -- ^ number of resampling points in SMC
                    -> Int -- ^ number of particles in SMC
                    -> Int -- ^ number of independent SMC runs
                    -> Sequential (Population (Weighted m)) a -> m [a]
pimh k np ns d = mhPrior ns $ collapse $ smcMultinomial k np d



data Kernel m a =
  Kernel {proposal :: a -> m a, density :: a -> a -> LogDomain (CustomReal m)}

mhCustom :: MonadDist m => Int -> Conditional (Weighted m) a -> Kernel m (Trace (CustomReal m)) -> Trace (CustomReal m) -> m [a]
mhCustom n model kernel starting = evalStateT (sequence $ replicate n $ mhCustomStep model kernel) starting where

mhCustomStep :: MonadDist m => Conditional (Weighted m) a -> Kernel m (Trace (CustomReal m)) -> StateT (Trace (CustomReal m)) m a
mhCustomStep model kernel = do
  t <- get
  t' <- lift $ proposal kernel t
  p <- lift $ unsafeDensity model t
  let q = density kernel t t'
  p' <- lift $ unsafeDensity model t'
  let q' = density kernel t' t
  let ratio = min 1 (p' * q' / (p * q))
  accept <- bernoulli (fromLogDomain ratio)
  let tnew = if accept then t' else t
  put tnew
  (output,_) <- lift $ runWeighted $ unsafeConditional model tnew
  return output

gaussianKernel :: (MonadDist m, CustomReal m ~ Double) => Double -> Kernel m Double
gaussianKernel sigma =
  Kernel (\x -> normal x sigma) (\x y -> pdf (normalDist x sigma) y)

singleSiteKernel :: (MonadDist m, Eq a) => Kernel m a -> Kernel m [a]
singleSiteKernel k = Kernel prop den where
  prop xs = if null xs then return [] else do
    index <- uniformD [0 .. length xs - 1]
    let (a, x:b) = splitAt index xs
    x' <- proposal k x
    return (a ++ (x':b))

  den xs ys | (length xs == length ys) =
    let
      n = length xs
      diffs = filter (uncurry (/=)) $ zip xs ys
    in
      if n == 0 then 1 else
        case length diffs of
          0 -> sum (zipWith (density k) xs ys) / fromIntegral n
          1 -> let [(x,y)] = diffs in density k x y / fromIntegral n
          _ -> error "Single site kernel was given lists differing in more than one element"

productKernel :: (MonadDist m, Eq a, Eq b)
              => CustomReal m -> Kernel m a -> Kernel m b -> Kernel m (a,b)
productKernel p k l = Kernel prop den where
  prop (a,b) = do
    takeA <- bernoulli p
    a' <- if takeA then proposal k a else return a
    b' <- if takeA then return b else proposal l b
    return (a',b')

  den (a,b) (a',b') = let p' = toLogDomain p in
    case (a == a', b == b') of
      (True, True)  -> p' * (density k a a') + (1-p') * (density l b b')
      (False, True) -> p' * density k a a'
      (True, False) -> (1-p') * density l b b'
      (False, False) -> error "Product kernel was given tuples with both elements different"

customDiscreteKernel :: MonadDist m => (Int -> [(CustomReal m)]) -> Kernel m Int
customDiscreteKernel f = Kernel (discrete . f) (\x y -> toLogDomain (f x !! y))

singleSiteTraceKernel :: (MonadDist m)
              => CustomReal m -> Kernel m (CustomReal m) -> Kernel m Int
              -> Kernel m (Trace (CustomReal m))
singleSiteTraceKernel p k l = Kernel prop den where
    prop t = do
      x <- proposal prodKer (toLists t)
      return (fromLists x)

    den t t' = density prodKer (toLists t) (toLists t')

    prodKer = productKernel p (singleSiteKernel k) (singleSiteKernel l)

randomWalkKernel :: (MonadDist m, CustomReal m ~ Double) => Double -> Kernel m (Trace Double)
randomWalkKernel sigma = singleSiteTraceKernel 1 (gaussianKernel sigma) undefined
