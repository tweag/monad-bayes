{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
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
<<<<<<< HEAD
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted (Weighted, runWeighted)
=======
import Control.Monad.Bayes.Class (MonadSample (bernoulli, random, uniformD))
import Control.Monad.Bayes.Weighted (Weighted, weighted)
>>>>>>> api
import Control.Monad.Trans.Cont
  ( Cont,
    ContT (ContT),
    cont,
    runCont,
  )
import Data.Foldable (Foldable (foldl'))
import Data.Set (Set, elems)
import Data.Text qualified as T
import Numeric.Integration.TanhSinh (Result (result), trap)
import Numeric.Log (Log (ln))
import Statistics.Distribution (density)
import Statistics.Distribution.Uniform qualified as Statistics

newtype Integrator n a = Integrator {getCont :: Cont n a}
  deriving newtype (Functor, Applicative, Monad)

<<<<<<< HEAD
runIntegrator :: (a -> Double) -> Integrator Double a -> Double
runIntegrator f (Integrator a) = runCont a f
=======
integrator, runIntegrator :: (a -> Double) -> Integrator a -> Double
integrator f (Integrator a) = runCont a f
runIntegrator = integrator
>>>>>>> api

instance (n ~ Double) => MonadSample n Integrator where
  randomGeneric = fromDensityFunction $ density $ Statistics.uniformDistr 0 1
  bernoulli p = Integrator $ cont (\f -> p * f True + (1 - p) * f False)

-- uniformD ls = fromMassFunction (const (1 / fromIntegral (length ls))) ls

fromDensityFunction :: (Double -> Double) -> Integrator Double Double
fromDensityFunction d = Integrator $
  cont $ \f ->
    integralWithQuadrature (\x -> f x * d x)
  where
    integralWithQuadrature = result . last . (\z -> trap z 0 1)

fromMassFunction :: Foldable f => (a -> Double) -> f a -> Integrator Double a
fromMassFunction f support = Integrator $ cont \g ->
  foldl' (\acc x -> acc + f x * g x) 0 support

empirical :: Foldable f => f a -> Integrator Double a
empirical = Integrator . cont . flip weightedAverage
  where
    weightedAverage :: (Foldable f, Fractional r) => (a -> r) -> f a -> r
    weightedAverage f = Foldl.fold (weightedAverageFold f)

    weightedAverageFold :: Fractional r => (a -> r) -> Fold a r
    weightedAverageFold f = Foldl.premap f averageFold

    averageFold :: Fractional a => Fold a a
    averageFold = (/) <$> Foldl.sum <*> Foldl.genericLength

<<<<<<< HEAD
expectation :: Integrator Double Double -> Double
expectation = runIntegrator id

variance :: Integrator Double Double -> Double
variance nu = runIntegrator (^ 2) nu - expectation nu ^ 2

momentGeneratingFunction :: Integrator Double Double -> Double -> Double
momentGeneratingFunction nu t = runIntegrator (\x -> exp (t * x)) nu
=======
expectation :: Integrator Double -> Double
expectation = integrator id

variance :: Integrator Double -> Double
variance nu = integrator (^ 2) nu - expectation nu ^ 2

momentGeneratingFunction :: Integrator Double -> Double -> Double
momentGeneratingFunction nu t = integrator (\x -> exp (t * x)) nu
>>>>>>> api

cumulantGeneratingFunction :: Integrator Double Double -> Double -> Double
cumulantGeneratingFunction nu = log . momentGeneratingFunction nu

normalize :: (n ~ Double) => Weighted Integrator n a -> Integrator Double a
normalize m =
  let m' = weighted m
      z = integrator (ln . exp . snd) m'
   in do
        (x, d) <- weighted m
        Integrator $ cont $ \f -> (f () * (ln $ exp d)) / z
        return x

<<<<<<< HEAD
cdf :: Integrator Double Double -> Double -> Double
cdf nu x = runIntegrator (negativeInfinity `to` x) nu
=======
cdf :: Integrator Double -> Double -> Double
cdf nu x = integrator (negativeInfinity `to` x) nu
>>>>>>> api
  where
    negativeInfinity :: Double
    negativeInfinity = negate (1 / 0)

    to :: (Num a, Ord a) => a -> a -> a -> a
    to a b k
      | k >= a && k <= b = 1
      | otherwise = 0

<<<<<<< HEAD
volume :: Integrator Double Double -> Double
volume = runIntegrator (const 1)
=======
volume :: Integrator Double -> Double
volume = integrator (const 1)
>>>>>>> api

containing :: (Num a, Eq b) => [b] -> b -> a
containing xs x
  | x `elem` xs = 1
  | otherwise = 0

instance Num a => Num (Integrator Double a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

<<<<<<< HEAD
probability :: Ord a => (a, a) -> Integrator Double a -> Double
probability (lower, upper) = runIntegrator (\x -> if x < upper && x >= lower then 1 else 0)

enumerateWith :: Ord a => Set a -> Integrator Double a -> [(a, Double)]
enumerateWith ls meas =
=======
probability :: Ord a => (a, a) -> Integrator a -> Double
probability (lower, upper) = integrator (\x -> if x < upper && x >= lower then 1 else 0)

enumeratorWith :: Ord a => Set a -> Integrator a -> [(a, Double)]
enumeratorWith ls meas =
>>>>>>> api
  [ ( val,
      integrator
        (\x -> if x == val then 1 else 0)
        meas
    )
    | val <- elems ls
  ]

histogram ::
  (Enum a, Ord a, Fractional a, n ~ Double) =>
  Int ->
  a ->
  Weighted Integrator Double a ->
  [(a, Double)]
histogram nBins binSize model = do
  x <- take nBins [1 ..]
  let transform k = (k - (fromIntegral nBins / 2)) * binSize
  return
    ( (fst)
        (transform x, transform (x + 1)),
      probability (transform x, transform (x + 1)) $ normalize model
    )

plotCdf :: Int -> Double -> Integrator Double Double -> [(T.Text, Double)]
plotCdf nBins binSize model = do
  x <- take nBins [1 ..]
  let transform k = (k - (fromIntegral nBins / 2)) * binSize
  return ((T.pack . show) $ transform x, cdf model (transform x))
