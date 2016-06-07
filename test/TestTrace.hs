{-# LANGUAGE
  GADTs
 #-}
 module TestTrace where

import System.Random
import Data.AEq
import Data.Maybe
import Data.Typeable
import Data.Number.LogFloat

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Prior
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Trace

import qualified Sprinkler

g = mkStdGen 0

extractNormal :: Cache -> Maybe Double
extractNormal (Cache (Normal _ _) x) = Just x
extractNormal _ = Nothing

extractBool :: Cache -> Maybe Bool
extractBool (Cache (Categorical _) x) = cast x
extractBool _ = Nothing

sprinkler :: MonadDist m => m Bool
sprinkler = prior Sprinkler.soft

m :: MonadDist m => m (Bool,Double)
m = do
  x <- bernoulli 0.5
  y <- normal 0 1
  return (x,y)

compare :: ((Bool,Double), [Cache]) -> Bool
compare ((x,y), cs) = fromMaybe False b where
  b = case cs of [c1,c2] -> do
                              x' <- extractBool c1
                              y' <- extractNormal c2
                              return (x == x' && y == y')
                 _ -> Nothing

withCache modifiedModel = do
  MHState snapshots weight answer <- modifiedModel
  return (answer, map snapshotToCache snapshots)

check_writing = TestTrace.compare (stdSample (withCache (mhState m)) g)

check_reading = stdSample (fmap snd $ withCache (fmap snd $ mhReuse caches m)) g == caches where
  caches = [ Cache (Categorical [(True, 0.5), (False, 0.5)]) True
           , Cache (Normal 0 1) 9000
           ]

check_reuse_ratio m = fromLogFloat (stdSample (fmap fst (mhReuse [] m)) g) ~== 1
