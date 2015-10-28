{-# LANGUAGE
  TupleSections
 #-}

module Importance where

import Control.Arrow (first,second)

import Base
import Explicit
import Dist

type Samples a = [(a,Prob)]
resample :: Samples a -> Dist (Samples a)
resample xs = sequence $ replicate n $ fmap (,1) $ categorical xs where
    n = length xs
flatten :: Samples (Samples a) -> Samples a
flatten xss = [(x,p*q) | (xs,p) <- xss, (x,q) <- xs]

importance :: Int -> Dist a -> Dist (Samples a)
importance n d = sequence $ replicate n $ prior d

importance' :: Int -> Dist a -> Dist a
importance' n d = importance n d >>= categorical

normalize :: Samples a -> Samples a
normalize xs = map (second (/ norm)) xs where
    norm = sum $ map snd xs
