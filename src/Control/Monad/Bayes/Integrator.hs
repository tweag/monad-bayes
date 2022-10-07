{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- This is adapted from https://jtobin.io/giry-monad-implementation
-- but brought into the monad-bayes framework (i.e. Integrator is an instance of MonadInfer)
-- It's largely for debugging other inference methods and didactic use,
-- because brute force integration of measures is
-- only practical for small programs
module Control.Monad.Bayes.Integrator
  ( probability,
    variance,
    expectation,
    cdf,
    empirical,
    enumeratorWith,
    histogram,
    plotCdf,
    volume,
    normalize,
    Integrator,
    momentGeneratingFunction,
    cumulantGeneratingFunction,
    integrator,
    runIntegrator,
  )
where

import Control.Applicative (Applicative (..))
import Control.Foldl (Fold)
import Control.Foldl qualified as Foldl
import Control.Monad.Bayes.Class (MonadSample (bernoulli, random, uniformD))
import Control.Monad.Bayes.Weighted (Weighted, weighted)
import Control.Monad.Cont
  ( Cont,
    ContT (ContT),
    cont,
    runCont,
  )
import Data.Foldable (Foldable (foldl'))
import Data.Set (Set, elems)
import Numeric.Integration.TanhSinh (Result (result), trap)
import Numeric.Log (Log (ln))
import Statistics.Distribution qualified as Statistics
import Statistics.Distribution.Uniform qualified as Statistics

newtype Integrator a = Integrator {getCont :: Cont Double a}
  deriving newtype (Functor, Applicative, Monad)

integrator, runIntegrator :: (a -> Double) -> Integrator a -> Double
integrator f (Integrator a) = runCont a f
runIntegrator = integrator

instance MonadSample Integrator where
  random = fromDensityFunction $ Statistics.density $ Statistics.uniformDistr 0 1
  bernoulli p = Integrator $ cont (\f -> p * f True + (1 - p) * f False)
  uniformD ls = fromMassFunction (const (1 / fromIntegral (length ls))) ls

fromDensityFunction :: (Double -> Double) -> Integrator Double
fromDensityFunction d = Integrator $
  cont $ \f ->
    integralWithQuadrature (\x -> f x * d x)
  where
    integralWithQuadrature = result . last . (\z -> trap z 0 1)

fromMassFunction :: Foldable f => (a -> Double) -> f a -> Integrator a
fromMassFunction f support = Integrator $ cont \g ->
  foldl' (\acc x -> acc + f x * g x) 0 support

empirical :: Foldable f => f a -> Integrator a
empirical = Integrator . cont . flip weightedAverage
  where
    weightedAverage :: (Foldable f, Fractional r) => (a -> r) -> f a -> r
    weightedAverage f = Foldl.fold (weightedAverageFold f)

    weightedAverageFold :: Fractional r => (a -> r) -> Fold a r
    weightedAverageFold f = Foldl.premap f averageFold

    averageFold :: Fractional a => Fold a a
    averageFold = (/) <$> Foldl.sum <*> Foldl.genericLength

expectation :: Integrator Double -> Double
expectation = integrator id

variance :: Integrator Double -> Double
variance nu = integrator (^ 2) nu - expectation nu ^ 2

momentGeneratingFunction :: Integrator Double -> Double -> Double
momentGeneratingFunction nu t = integrator (\x -> exp (t * x)) nu

cumulantGeneratingFunction :: Integrator Double -> Double -> Double
cumulantGeneratingFunction nu = log . momentGeneratingFunction nu

normalize :: Weighted Integrator a -> Integrator a
normalize m =
  let m' = weighted m
      z = integrator (ln . exp . snd) m'
   in do
        (x, d) <- weighted m
        Integrator $ cont $ \f -> (f () * (ln $ exp d)) / z
        return x

cdf :: Integrator Double -> Double -> Double
cdf nu x = integrator (negativeInfinity `to` x) nu
  where
    negativeInfinity :: Double
    negativeInfinity = negate (1 / 0)

    to :: (Num a, Ord a) => a -> a -> a -> a
    to a b k
      | k >= a && k <= b = 1
      | otherwise = 0

volume :: Integrator Double -> Double
volume = integrator (const 1)

containing :: (Num a, Eq b) => [b] -> b -> a
containing xs x
  | x `elem` xs = 1
  | otherwise = 0

instance Num a => Num (Integrator a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

probability :: Ord a => (a, a) -> Integrator a -> Double
probability (lower, upper) = integrator (\x -> if x < upper && x >= lower then 1 else 0)

enumeratorWith :: Ord a => Set a -> Integrator a -> [(a, Double)]
enumeratorWith ls meas =
  [ ( val,
      integrator
        (\x -> if x == val then 1 else 0)
        meas
    )
    | val <- elems ls
  ]

histogram ::
  (Enum a, Ord a, Fractional a) =>
  Int ->
  a ->
  Weighted Integrator a ->
  [(a, Double)]
histogram nBins binSize model = do
  x <- take nBins [1 ..]
  let transform k = (k - (fromIntegral nBins / 2)) * binSize
  return
    ( (fst)
        (transform x, transform (x + 1)),
      probability (transform x, transform (x + 1)) $ normalize model
    )

plotCdf :: Int -> Double -> Double -> Integrator Double -> [(Double, Double)]
plotCdf nBins binSize middlePoint model = do
  x <- take nBins [1 ..]
  let transform k = (k - (fromIntegral nBins / 2)) * binSize + middlePoint
  return (transform x, cdf model (transform x))
