{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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
    enumerate,
    expectation,
    normalForm,
    toBin,
    empirical
  )
where

import Control.Applicative (Alternative)
import Control.Arrow (second)
import Control.Monad (MonadPlus, when)
import Control.Monad.Bayes.Class
import Control.Monad.Trans.Writer
import Data.AEq (AEq, (===), (~==))
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Vector.Generic as V
import Numeric.Log as Log
import qualified Data.Text as T
import qualified Data.Vector as VV
import Control.Monad.Except (runExcept, MonadError (throwError))
import Data.Fixed (mod')

-- | An exact inference transformer that integrates
-- discrete random variables by enumerating all execution paths.
newtype Enumerator a = Enumerator (WriterT (Product (Log Double)) [] a)
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadSample Enumerator where
  random = error "Infinitely supported random variables not supported in Enumerator"
  bernoulli p = fromList [(True, (Exp . log) p), (False, (Exp . log) (1 - p))]
  categorical v = fromList $ zip [0 ..] $ map (Exp . log) (V.toList v)

instance MonadCond Enumerator where
  score w = fromList [((), w)]

instance MonadInfer Enumerator

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
    m = enumerate d

-- | Aggregate weights of equal values.
-- The resulting list is sorted ascendingly according to values.
compact :: (Num r, Ord a) => [(a, r)] -> [(a, r)]
compact = Map.toAscList . Map.fromListWith (+)

-- | Aggregate and normalize of weights.
-- The resulting list is sorted ascendingly according to values.
--
-- > enumerate = compact . explicit
enumerate :: Ord a => Enumerator a -> [(a, Double)]
enumerate d = compact (zip xs ws)
  where
    (xs, ws) = second (map (exp . ln) . normalize) $ unzip (logExplicit d)

-- | Expectation of a given function computed using normalized weights.
expectation :: (a -> Double) -> Enumerator a -> Double
expectation f = Prelude.sum . map (\(x, w) -> f x * (exp . ln) w) . normalizeWeights . logExplicit

normalize :: [Log Double] -> [Log Double]
normalize xs = map (/ z) xs
  where
    z = Log.sum xs

-- | Divide all weights by their sum.
normalizeWeights :: [(a, Log Double)] -> [(a, Log Double)]
normalizeWeights ls = zip xs ps
  where
    (xs, ws) = unzip ls
    ps = normalize ws

-- | 'compact' followed by removing values with zero weight.
normalForm :: Ord a => Enumerator a -> [(a, Double)]
normalForm = filter ((/= 0) . snd) . compact . explicit

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


-- | The empirical distribution of a set of weighted samples
empirical :: Ord a => [(a, Double)] -> Either T.Text [(a, Double)]
empirical samples = runExcept $ do
    let (support, probs) = unzip samples
    when (any (<= 0) probs) (throwError "Probabilities are not all strictly positive")
    return $ enumerate $ fromList (second (log . Exp) <$> samples)
    -- do
    --   i <- categorical $ VV.fromList probs
    --   return $ support !! i


type Bin = (Double, Double)
-- | binning function. Useful when you want to return the bin that
-- a random variable falls into, so that you can show a histogram of samples
toBin :: Double -- ^ bin size 
  -> Double -- ^ number
  -> Bin
toBin binSize n = let lb = n `mod'` binSize in (n-lb, n-lb + binSize) 