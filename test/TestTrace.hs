{-# LANGUAGE
  GADTs
 #-}
 module TestTrace where

-- Some of the type annotations in this file are no longer required

import Data.AEq
import Data.Maybe
import Control.Monad

import Control.Monad.Bayes.LogDomain (LogDomain, toLogDomain, fromLogDomain, toLog)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Prior
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Trace

-- extractNormal :: Cache Double -> Maybe Double
-- extractNormal (Cache (Continuous (Normal _ _)) x) = Just (realToFrac x)
-- extractNormal _ = Nothing
--
-- extractInt :: Cache Double -> Maybe Int
-- extractInt (Cache (Discrete _) x) = Just (fromIntegral x)
-- extractInt _ = Nothing

discreteWeights :: Fractional a => [a]
discreteWeights = [0.0001, 0.9999]

m :: (MonadDist m, CustomReal m ~ Double) => m (Int, Double)
m = do
  x <- discrete discreteWeights
  y <- normal 0 1
  return (x,y)
--
-- compare :: ((Int,Double), [Cache Double]) -> Bool
-- compare ((x,y), cs) = fromMaybe False b where
--   b = case cs of [c1,c2] -> do
--                               x' <- extractInt    c1
--                               y' <- extractNormal c2
--                               return (x == x' && y == y')
--                  _ -> Nothing
--
-- withCache modifiedModel = do
--   MHState snapshots weight answer <- modifiedModel
--   return (answer, map snapshotToCache snapshots)
--
-- check_writing = TestTrace.compare (stdSample (withCache (mhState m)) g)
--
-- check_reading = stdSample (fmap snd $ withCache (fmap snd $ mhReuse caches m)) g == caches where
--   caches = [ Cache (Discrete discreteWeights) (0 :: Int)
--            , Cache (Continuous (Normal 0 1)) (9000 :: Double)
--            ]
--
-- check_reuse_ratio m = fromLogDomain (stdSample (fmap fst (mhReuse [] m)) g) ~== 1
