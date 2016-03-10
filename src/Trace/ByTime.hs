{-# LANGUAGE FlexibleInstances #-}

module Trace.ByTime where

------------------------------------
-- RANDOM CHOICES INDEXED BY TIME --
------------------------------------

import Base
import Primitive
import Trace

empty :: [Cache]
empty = []

instance RandomDB [Cache] where
  singleton = (: [])
  consult _ []       = Nothing
  consult _ (x : xs) = Just (x, xs)
  mutate [] = return []
  mutate xs = do
    i <- uniformD [0 .. length xs - 1]
    let (prefix, cache : postfix) = splitAt i xs
    cache' <- refresh cache
    return $ prefix ++ cache' : postfix
  size = length
  weight = foldr (\(Cache d x) p -> pdf d x * p) 1
  resampled old [] = []
  resampled [] new = new
  resampled (Cache d x : old) (Cache d' x' : new) | reusedSample d x d' x' = resampled old new
  resampled (cache : old) (cache' : new) = cache' : resampled old new
