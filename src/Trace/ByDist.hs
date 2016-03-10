{-# LANGUAGE
  FlexibleContexts,
  GeneralizedNewtypeDeriving,
  GADTs
 #-}

module Trace.ByDist where

--------------------------------------------
-- RANDOM CHOICES INDEXED BY DISTRIBUTION --
--------------------------------------------

import Control.Arrow
import Data.List
import Data.Typeable

import Base
import Primitive
import Trace

import Trace.ByTime hiding (empty)

newtype ByDist = ByDist { runByDist :: [(Prim, Cache)] }
  deriving (Monoid)

empty :: ByDist
empty = ByDist []

data Prim where
  Prim :: Primitive a -> Prim

instance Eq Prim where
  Prim (Categorical xs) == Prim (Categorical ys) = maybe False (== ys) (cast xs)
  Prim (Normal m s    ) == Prim (Normal m' s'  ) = m == m' && s == s'
  Prim (Gamma  a b    ) == Prim (Gamma  a' b'  ) = a == a' && b == b'
  Prim (Beta   a b    ) == Prim (Beta   a' b'  ) = a == a' && b == b'
  Prim (_             ) == Prim (_             ) = False

partitionAt :: (Eq a) => a -> [(a, b)] -> ([(a, b)], [(a, b)])
partitionAt key = partition ((== key) . fst)

instance RandomDB ByDist where
  record d x = ByDist [(Prim d, Cache d x)]

  consult d (ByDist xs) =
    let
      (matched, notMatched) = partitionAt (Prim d) xs
    in
      if null matched then
        Nothing
      else
        Just (snd $ head $ matched, ByDist $ tail matched ++ notMatched)

  size (ByDist xs) = length xs

  weight (ByDist xs) = weight $ map snd xs

  resampled (ByDist xs) (ByDist ys) =
    ByDist $ loop (nub (map fst ys)) xs ys
    where
      loop [] xs ys = []
      loop (key : keys) xs ys =
        let
          (kx, xs') = partitionAt key xs
          (ky, ys') = partitionAt key ys
        in
          (map ((,) key) $ resampled (map snd kx) (map snd ky)) ++ loop keys xs' ys'

  mutate (ByDist xs) = do
    let keys = map fst xs
    caches' <- mutate $ map snd xs
    return $ ByDist $ zip keys caches'
