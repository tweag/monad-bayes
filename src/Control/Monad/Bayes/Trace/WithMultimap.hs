{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  GADTs
 #-}

module Control.Monad.Bayes.Trace.WithMultimap where

--------------------------------------------
-- RANDOM CHOICES INDEXED BY DISTRIBUTION --
--------------------------------------------

import Control.Arrow
import Data.List
import Data.Typeable

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Trace

import Control.Monad.Bayes.Trace.ByTime hiding (empty)

class (Eq k) => MultimapKey k where
  mkKey :: Primitive a -> k

partitionAt :: (Eq a) => a -> [(a, b)] -> ([(a, b)], [(a, b)])
partitionAt key = partition ((== key) . fst)

instance (MultimapKey k) => RandomDB [(k, Cache)] where

  record d x = [(mkKey d, Cache d x)]

  consult d xs =
    let
      (matched, notMatched) = partitionAt (mkKey d) xs
    in
      if null matched then
        Nothing
      else
        Just (snd $ head $ matched, tail matched ++ notMatched)

  size = length

  weight xs = weight $ map snd xs

  resampled xs ys = loop (nub (map fst ys)) xs ys
    where
      loop [] xs ys = []
      loop (key : keys) xs ys =
        let
          (kx, xs') = partitionAt key xs
          (ky, ys') = partitionAt key ys
        in
          (map ((,) key) $ resampled (map snd kx) (map snd ky)) ++ loop keys xs' ys'

  mutate xs = do
    let keys = map fst xs
    caches' <- mutate $ map snd xs
    return $ zip keys caches'
