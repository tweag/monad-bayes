{-# LANGUAGE
  GADTs,
  FlexibleInstances
 #-}

module Trace.ByDistribution where

--------------------------------------------
-- RANDOM CHOICES INDEXED BY DISTRIBUTION --
--------------------------------------------

-- Caution:
-- Implementation is wrong at the moment.
-- Consider using Data.List.Utils for multimap methods.

import Control.Arrow
import Data.Typeable

import Base
import Primitive
import Trace

import Trace.ByTime hiding (empty)

newtype Indexed = Indexed { runIndexed :: [(Prim, [Cache])] }

empty :: Indexed
empty = Indexed []

data Prim where
  Prim :: Primitive a -> Prim

instance Eq Prim where
  Prim (Categorical xs) == Prim (Categorical ys) = maybe False (== ys) (cast xs)
  Prim (Normal m s    ) == Prim (Normal m' s'  ) = m == m' && s == s'
  Prim (Gamma  a b    ) == Prim (Gamma  a' b'  ) = a == a' && b == b'
  Prim (Beta   a b    ) == Prim (Beta   a' b'  ) = a == a' && b == b'
  Prim (_             ) == Prim (_             ) = False

seek :: (Eq a) => a -> [(a, b)] -> Maybe (b, [(a, b)])
seek x [] = Nothing
seek x ((x', y) : ys) | x == x' = Just (y, ys)
seek x ((x', y) : ys) = fmap (second ((x', y) :)) (seek x ys)

instance Monoid Indexed where
  mempty = empty
  mappend (Indexed []) (Indexed ys) = Indexed ys
  mappend (Indexed ((d, cs) : xs)) (Indexed ys) =
    case seek d ys of
      Nothing -> Indexed $ (d, cs) : runIndexed (mappend (Indexed xs) (Indexed ys))
      Just (cs', ys') -> Indexed $ (d, cs ++ cs') : runIndexed (mappend (Indexed xs) (Indexed ys'))

instance RandomDB Indexed where
  record d x = Indexed [(Prim d, [Cache d x])]
  consult d (Indexed xs) = do
    (cs, xs') <- seek (Prim d) xs
    if null cs then
      Nothing
    else
      Just (head cs, Indexed $ (Prim d, tail cs) : xs')

  size (Indexed xs) = sum $ map (length . snd) xs

  weight (Indexed xs) = weight $ concat (map snd xs)

  resampled (Indexed xs) (Indexed ys) = Indexed $ loop xs ys
    where
    loop xs [] = []
    loop xs ((d, cs) : ys) =
      case seek d xs of
        Nothing -> (d, cs) : loop xs ys
        Just (cs', xs') ->
          let
            re_cs = resampled cs cs'
          in
            if null re_cs then loop xs' ys else (d, re_cs) : loop xs' ys

  mutate xs = do
    i <- uniformD [0 .. size xs - 1]
    xs' <- refreshAt i (runIndexed xs)
    return $ Indexed xs'
    where
      refreshAt 0 ((d, c : cs) : xs) = do
        c' <- refresh c
        return $ (d, c' : cs) : xs
      refreshAt n ((d, []) : xs) = do
        xs' <- refreshAt n xs
        return $ (d, []) : xs'
      refreshAt n ((d, c : cs) : xs) = do
        (_, cs') : xs' <- refreshAt (n - 1) ((d, cs) : xs)
        return $ (d, c : cs') : xs'
