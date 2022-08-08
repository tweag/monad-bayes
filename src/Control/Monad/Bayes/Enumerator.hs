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
    -- enumerateToDistribution,
    removeZeros,
    fromList,
  )
where

import Control.Applicative (Alternative)
import Control.Arrow (second)
import Control.Monad (MonadPlus)
import Control.Monad.Bayes.Class
import Control.Monad.Trans.Writer (WriterT (..))
import Data.AEq (AEq, (===), (~==))
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Product (..))
import Data.Ord (Down (Down))
import Data.Vector qualified as VV
import Data.Vector.Generic qualified as V
import Numeric.Log as Log (Log (..), sum)
import Data.Number.Erf

-- | An exact inference transformer that integrates
-- discrete random variables by enumerating all execution paths.
newtype Enumerator n a = Enumerator (WriterT (Product (Log n)) [] a)
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus)

instance (RealFloat n, InvErf n) => MonadSample n (Enumerator) where
  randomGeneric = error "Infinitely supported random variables not supported in Enumerator"

-- bernoulli p = fromList [(True, (Exp . log) p), (False, (Exp . log) (1 - p))]
-- categorical v = fromList $ zip [0 ..] $ map (Exp . log) (V.toList v)

instance RealFloat n => MonadCond n (Enumerator) where
  scoreGeneric w = fromList [((), w)]

instance (RealFloat n, InvErf n) => MonadInfer n (Enumerator)

-- | Construct Enumerator from a list of values and associated weights.
fromList :: [(a, Log n)] -> Enumerator n a
fromList = Enumerator . WriterT . map (second Product)

-- | Returns the posterior as a list of weight-value pairs without any post-processing,
-- such as normalization or aggregation
logExplicit :: Enumerator n a -> [(a, Log n)]
logExplicit (Enumerator m) = map (second getProduct) $ runWriterT m

-- | Same as `toList`, only weights are converted from log-domain.
explicit :: RealFloat n => Enumerator n a -> [(a, n)]
explicit = map (second (exp . ln)) . logExplicit

-- | Returns the model evidence, that is sum of all weights.
evidence :: RealFloat n => Enumerator n a -> Log n
evidence = Log.sum . map snd . logExplicit

-- | Normalized probability mass of a specific value.
mass :: (Ord a, Num n, RealFloat n) => Enumerator n a -> a -> n
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
enumerator, enumerate :: (RealFloat n, Ord a) => Enumerator n a -> [(a, n)]
enumerator d = filter ((/= 0) . snd) $ compact (zip xs ws)
  where
    (xs, ws) = second (map (exp . ln) . normalize) $ unzip (logExplicit d)

-- | deprecated synonym
enumerate = enumerator

-- | Expectation of a given function computed using normalized weights.
expectation :: RealFloat n => (a -> n) -> Enumerator n a -> n
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
normalForm :: RealFloat n => Ord a => Enumerator n a -> [(a, n)]
normalForm = filter ((/= 0) . snd) . compact . explicit

toEmpirical :: (Fractional b, Ord a, Ord b) => [a] -> [(a, b)]
toEmpirical ls = normalizeWeights $ compact (zip ls (repeat 1))

toEmpiricalWeighted :: (Fractional b, Ord a, Ord b) => [(a, b)] -> [(a, b)]
toEmpiricalWeighted = normalizeWeights . compact

-- enumerateToDistribution :: (RealFloat n, MonadSample n m) => Enumerator n a -> m a
-- enumerateToDistribution model = do
--   let samples = logExplicit model
--   let (support, logprobs) = unzip samples
--   i <- logCategorical $ VV.fromList logprobs
--   return $ support !! i

removeZeros :: RealFloat n => Enumerator n a -> Enumerator n a
removeZeros (Enumerator (WriterT a)) = Enumerator $ WriterT $ filter ((\(Product x) -> x /= 0) . snd) a

instance (Ord a, RealFloat n) => Eq (Enumerator n a) where
  p == q = normalForm p == normalForm q

instance (RealFloat n, Ord a, AEq n) => AEq (Enumerator n a) where
  p === q = xs == ys && ps === qs
    where
      (xs, ps) = unzip (normalForm p)
      (ys, qs) = unzip (normalForm q)
  p ~== q = xs == ys && ps ~== qs
    where
      (xs, ps) = unzip $ filter (not . (~== 0) . snd) $ normalForm p
      (ys, qs) = unzip $ filter (not . (~== 0) . snd) $ normalForm q
