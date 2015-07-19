{-# LANGUAGE TupleSections #-}

module Explicit where

import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, sequenceA)
import Control.Applicative (Applicative, pure, (<*>), liftA)
import Control.Arrow (first, second)
import System.Random (randomR)
import Data.List (find, nub)
import qualified Data.Map as Map (fromListWith, toAscList)

import Base

-- | A datatype for explicit representation of a probability distribution
-- as a list of weighted values.
-- Closely follows Erwig and Kollmansberger's design.
-- https://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
newtype Explicit a = Explicit {toList :: [(a,Prob)]}
                deriving(Show)

-- | Multiplies all probabilities by a constant such that they sum to 1.
normalize :: [(a,Prob)] -> [(a,Prob)]
normalize xs = map (second (/ norm)) xs where
    norm = sum $ map snd xs
-- | Multiplies all probabilities by a value that depends on
-- the associated atom.
reweight :: (a -> Prob) -> [(a,Prob)] -> [(a,Prob)]
reweight f = map (\(x,p) -> (x, p * f x))

instance Functor Explicit where
    fmap f (Explicit xs) = Explicit $ map (first f) xs

instance Applicative Explicit where
    pure = return
    (Explicit fs) <*> (Explicit xs) =
      Explicit [(f x, p*q) | (f,p) <- fs, (x,q) <- xs]

instance Monad Explicit where
    return x = Explicit [(x, 1)]
    d >>= f  = djoin $ fmap f d

instance Foldable Explicit where
    foldMap f (Explicit ys) = foldMap f (map fst ys)

instance Traversable Explicit where
    sequenceA (Explicit ys) = liftA Explicit $ sequenceA $ fmap (\(d,p) -> liftA (,p) d) ys

instance DiscreteDist Explicit where
    categorical = Explicit . normalize

instance Conditional Explicit where
    condition c (Explicit xs) = Explicit $ normalize $ reweight c xs

instance Sampleable Explicit where
    sample g (Explicit xs) =
        pick xs $ fst $ randomR (0.0,1.0) g

instance Scoreable Explicit where
    score (Explicit xs) x =
        case lookup x xs of
          Just p -> p
          Nothing -> 0

---------------------------------------------------
-- Some useful functions

pick :: [(a,Prob)] -> Prob -> a
-- | Picks a value from a distribution using a real from [0,1].
pick ((x,p):ps) v = if v <= p then x else pick ps (v-p)
pick []         v = error $ "Sampling failed. Missing probability mass "
                    ++ show v

djoin :: Explicit (Explicit a) -> Explicit a
-- | Flattens a distribution over distributions.
djoin (Explicit ds) = Explicit $
                        [(x, p * q) | (Explicit xs,p) <- ds, (x,q) <- xs]

compact :: Eq a => Explicit a -> Explicit a
-- | Sums probabilities of duplicate elements.
-- Generality comes with a quadratic time cost.
compact (Explicit xs) =
  Explicit [(x, p x) | x <- distinct] where
    distinct = nub $ map fst xs
    p x = sum $ map snd $ filter ((== x) . fst) xs

kl :: Eq a => Explicit a -> Explicit a -> Double
-- | Computes the Kullback-Leibler divergence between two distributions.
-- Generality comes with a quadratic time cost.
kl d d' =
  sum $ map f xs where
    xs = toList $ compact d
    ys = toList $ compact d'
    f (x,p) = case find ((== x) . fst) ys of
      Just (y,q) -> toDouble p * (toLog p - toLog q)
      Nothing -> error "Undefined KL divergence - q is 0 when p is not"

fastCompact :: Ord a => Explicit a -> Explicit a
-- | Faster version of 'compact' for ordered types.
-- Internally uses a 'Map' to achieve O(n log n) runtime.
-- Returns an ascending list.
fastCompact = Explicit . Map.toAscList . Map.fromListWith (+) . toList

fastKL :: Ord a => Explicit a -> Explicit a -> Double
-- | Faster version of 'kl' for ordered types.
-- Internally uses a 'Map' to achieve O(n log n) runtime.
fastKL d d' =
  scan xs ys where
    xs = toList $ fastCompact d
    ys = toList $ fastCompact d'
    scan [] _ = 0
    scan ((x,p):xs) ((y,q):ys) =
      if x == y then
      toDouble p * (toLog p - toLog q) + scan xs ys
      else scan((x,p):xs) ys
    scan xs [] = error "Undefined KL divergence - q is 0 when p is not"

kl' :: Ord a => (a -> Prob) -> Explicit a -> Double
-- | Fast KL with an external function computing the reference probabilities.
kl' reference d = if result < 0 then error "Negative KL" else result where
    result = sum $ map component $ toList $ fastCompact d where
        component (x,p) = toDouble p * (toLog p - toLog q) where
            q' = reference x
            q  = if q' == 0 then error "Undefined KL divergence - q is 0 when p is not"
                 else q'
