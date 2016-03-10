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
import Trace.WithMultimap
import Trace.ByTime hiding (empty)

newtype ByDist = ByDist { runByDist :: [(Prim, Cache)] }
  deriving (Monoid)

empty :: ByDist
empty = ByDist []

-- Boilerplate. Cannot derive this instance due to nominal roles.
instance RandomDB ByDist where
  singleton = ByDist . singleton
  consult d = fmap (second ByDist) . consult d . runByDist
  mutate = fmap ByDist . mutate . runByDist
  size = size . runByDist
  weight = weight . runByDist
  resampled (ByDist xs) (ByDist ys) = ByDist $ resampled xs ys

data Prim where
  Prim :: Primitive a -> Prim

instance MultimapKey Prim where
  mkKey = Prim

instance Eq Prim where
  Prim (Categorical xs) == Prim (Categorical ys) = maybe False (== ys) (cast xs)
  Prim (Normal m s    ) == Prim (Normal m' s'  ) = m == m' && s == s'
  Prim (Gamma  a b    ) == Prim (Gamma  a' b'  ) = a == a' && b == b'
  Prim (Beta   a b    ) == Prim (Beta   a' b'  ) = a == a' && b == b'
  Prim (_             ) == Prim (_             ) = False
