{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module      : Control.Monad.Bayes.Enumerator
-- Description : Exhaustive enumeration of discrete random variables
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Enumerator
  ( Enumerator,
    logExplicit,
    explicit,
    evidence,
    mass,
    compact,
    enumerator,
    enumerate,
    expectation,
    normalForm,
    toEmpirical,
    toEmpiricalWeighted,
    normalizeWeights,
    enumerateToDistribution,
    removeZeros,
    fromList,
  )
where

import Control.Applicative (Alternative)
import Control.Arrow (second)
import Control.Monad.Bayes.Class
  ( MonadFactor (..),
    MonadMeasure,
    MonadDistribution (bernoulli, categorical, logCategorical, random),
  )
import Control.Monad.Writer
import Data.AEq (AEq, (===), (~==))
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import Data.Vector qualified as VV
import Data.Vector.Generic qualified as V
import Numeric.Log as Log (Log (..), sum)

-- | An exact inference transformer that integrates
-- discrete random variables by enumerating all execution paths.
newtype Enumerator a = Enumerator (WriterT (Product (Log Double)) [] a)
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadDistribution Enumerator where
  random = error "Infinitely supported random variables not supported in Enumerator"
  bernoulli p = fromList [(True, (Exp . log) p), (False, (Exp . log) (1 - p))]
  categorical v = fromList $ zip [0 ..] $ map (Exp . log) (V.toList v)

instance MonadFactor Enumerator where
  score w = fromList [((), w)]

instance MonadMeasure Enumerator

-- | Construct Enumerator from a list of values and associated weights.
fromList :: [(a, Log Double)] -> Enumerator a
fromList = Enumerator . WriterT . map (second Product)

-- | Returns the posterior as a list of weight-value pairs without any post-processing,
-- such as normalization or aggregation
logExplicit :: Enumerator a -> [(a, Log Double)]
logExplicit (Enumerator m) = map (second getProduct) $ runWriterT m

-- | Same as `toList`, only weights are converted from log-domain.
explicit :: Enumerator a -> [(a, Double)]
explicit = map (second (exp . ln)) . logExplicit

-- | Returns the model evidence, that is sum of all weights.
evidence :: Enumerator a -> Log Double
evidence = Log.sum . map snd . logExplicit

-- | Normalized probability mass of a specific value.
mass :: Ord a => Enumerator a -> a -> Double
mass d = f
  where
    f a = fromMaybe 0 $ lookup a m
    m = enumerator d

-- | Aggregate weights of equal values.
-- The resulting list is sorted ascendingly according to values.
compact :: (Num r, Ord a, Ord r) => [(a, r)] -> [(a, r)]
compact = sortOn (Down . snd) . Map.toAscList . Map.fromListWith (+)

-- | Aggregate and normalize of weights.
-- The resulting list is sorted ascendingly according to values.
--
-- > enumerator = compact . explicit
enumerator, enumerate :: Ord a => Enumerator a -> [(a, Double)]
enumerator d = filter ((/= 0) . snd) $ compact (zip xs ws)
  where
    (xs, ws) = second (map (exp . ln) . normalize) $ unzip (logExplicit d)

-- | deprecated synonym
enumerate = enumerator

-- | Expectation of a given function computed using normalized weights.
expectation :: (a -> Double) -> Enumerator a -> Double
expectation f = Prelude.sum . map (\(x, w) -> f x * (exp . ln) w) . normalizeWeights . logExplicit

normalize :: Fractional b => [b] -> [b]
normalize xs = map (/ z) xs
  where
    z = Prelude.sum xs

-- | Divide all weights by their sum.
normalizeWeights :: Fractional b => [(a, b)] -> [(a, b)]
normalizeWeights ls = zip xs ps
  where
    (xs, ws) = unzip ls
    ps = normalize ws

-- | 'compact' followed by removing values with zero weight.
normalForm :: Ord a => Enumerator a -> [(a, Double)]
normalForm = filter ((/= 0) . snd) . compact . explicit

toEmpirical :: (Fractional b, Ord a, Ord b) => [a] -> [(a, b)]
toEmpirical ls = normalizeWeights $ compact (zip ls (repeat 1))

toEmpiricalWeighted :: (Fractional b, Ord a, Ord b) => [(a, b)] -> [(a, b)]
toEmpiricalWeighted = normalizeWeights . compact

enumerateToDistribution :: (MonadDistribution n) => Enumerator a -> n a
enumerateToDistribution model = do
  let samples = logExplicit model
  let (support, logprobs) = unzip samples
  i <- logCategorical $ VV.fromList logprobs
  return $ support !! i

removeZeros :: Enumerator a -> Enumerator a
removeZeros (Enumerator (WriterT a)) = Enumerator $ WriterT $ filter ((\(Product x) -> x /= 0) . snd) a

instance Ord a => Eq (Enumerator a) where
  p == q = normalForm p == normalForm q

instance Ord a => AEq (Enumerator a) where
  p === q = xs == ys && ps === qs
    where
      (xs, ps) = unzip (normalForm p)
      (ys, qs) = unzip (normalForm q)
  p ~== q = xs == ys && ps ~== qs
    where
      (xs, ps) = unzip $ filter (not . (~== 0) . snd) $ normalForm p
      (ys, qs) = unzip $ filter (not . (~== 0) . snd) $ normalForm q
