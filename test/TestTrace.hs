{-# LANGUAGE
  GADTs
 #-}
 module TestTrace where

import System.Random
import Data.AEq
import Data.Maybe
import Data.Typeable

import Primitive
import Base
import Sampler
import Trace
import Trace.ByTime

g = mkStdGen 0

extractNormal :: Cache -> Maybe Double
extractNormal (Cache (Normal _ _) x) = Just x
extractNormal _ = Nothing

extractBool :: Cache -> Maybe Bool
extractBool (Cache (Categorical _) x) = cast x
extractBool _ = Nothing

m :: MonadDist m => m (Bool,Double)
m = do
  x <- bernoulli 0.5
  y <- normal 0 1
  return (x,y)

results = stdSample (runTraceT m) g
((x,y),r) = results

compare :: ((Bool,Double), [Cache]) -> Bool
compare ((x,y), cs) = fromMaybe False b where
  b = case cs of [c1,c2] -> do
                              x' <- extractBool c1
                              y' <- extractNormal c2
                              return (x == x' && y == y')
                 _ -> Nothing

check_writing = TestTrace.compare $ results

check_reading = x == x' && y == y' where
  (x',y') = fst $ stdSample (runReuseT m r) g
