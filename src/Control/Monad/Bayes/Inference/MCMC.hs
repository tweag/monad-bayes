{-|
Module      : Control.Monad.Bayes.Inference.MCMC
Description : Markov chain Monte Carlo
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Inference.MCMC (
  Kernel(Kernel),
  proposal,
  density,
  mhCustom,
  mhCustomStep,
  gaussianKernel,
  singleSiteKernel,
  productKernel,
  customDiscreteKernel,
  singleSiteTraceKernel,
  randomWalkKernel
) where

import Prelude hiding (sum)

import Control.Monad.State

import Numeric.LogDomain
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Conditional

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
  den xs ys =
    error $ "Single site kernel density: given lists have different lengths " ++ show (length xs) ++
            " and " ++ show (length ys)

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
