{-# LANGUAGE
  GADTs,
  GeneralizedNewtypeDeriving
 #-}

module Trace.ByType where

import Control.Arrow
import Data.Typeable

import Primitive
import Trace
import Trace.WithMultimap
import Trace.ByTime hiding (empty)

newtype ByType = ByType { runByType :: [(TypeRep, Cache)] }
  deriving (Monoid)

empty :: ByType
empty = ByType []

-- Boilerplate. Cannot derive this instance due to nominal roles.
instance RandomDB ByType where
  singleton = ByType . singleton
  consult d = fmap (second ByType) . consult d . runByType
  mutate = fmap ByType . mutate . runByType
  size = size . runByType
  weight = weight . runByType
  resampled (ByType xs) (ByType ys) = ByType $ resampled xs ys

instance MultimapKey TypeRep where
  mkKey (Normal m _)     = typeOf m
  mkKey (Gamma  a _)     = typeOf a
  mkKey (Beta   a _)     = typeOf a
  mkKey (Categorical xs) = typeOf xs

