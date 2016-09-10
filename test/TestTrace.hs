{-# LANGUAGE
  GADTs
 #-}
 module TestTrace where

-- Some of the type annotations in this file are no longer required

import System.Random
import Data.AEq
import Data.Maybe
import Control.Monad

import Control.Monad.Bayes.LogDomain (LogDomain, toLogDomain, fromLogDomain, toLog)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Dist
import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Prior
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Coprimitive

g = mkStdGen 0

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

conditional_m :: (MonadBayes m, CustomReal m ~ Double) => Maybe Int -> Maybe Double -> m (Int,Double)
conditional_m mx my = liftM2 (,) px py where
  px = case mx of
    Just x -> factor (if x == 0 then 0.0001 else if x == 1 then 0.9999 else 0)
      >> return x
    Nothing -> discrete discreteWeights
  py = case my of
    Just x -> factor (pdf (Continuous (Normal 0 1)) x) >> return x
    Nothing -> normal 0 1

check_conditional :: Maybe Int -> Maybe Double -> Bool
check_conditional mx my =
  enumerate (conditional m [fmap Left mx, fmap Right my]) ~==
    enumerate (conditional_m mx my)

check_empty_conditional = undefined

check_first_conditional = undefined

-- Computes conditional correctly when one of the values is empty
check_missing_conditional = check_conditional Nothing (Just 0.3)

-- Ignores values that don't match the type of required distribution
check_mismatch_conditional =
  enumerate (conditional m [Just (Right (0.4 :: Double)), Just (Right (0.3 ::Double))]) ~==
    enumerate (conditional_m Nothing (Just 0.3))

-- Computes conditional correctly in presence of more values than required
check_longer_conditional =
  enumerate (conditional m [Just (Left (1 :: Int)), Just (Right (0.3 ::Double)), Just (Right (undefined :: Double))]) ~==
    enumerate (conditional_m (Just 1) (Just 0.3))

-- Computes pseudomarginal density correctly
check_first_density =
  enumerate (fmap toLog (pseudoDensity m [Nothing, Just (Right (0.6 :: Double))])) ~==
    [(toLog (pdf (Continuous (Normal 0 1)) (0.6 :: Double)), 1)]

-- Computes joint density correctly when possible
check_joint_density_true =
  case jointDensity m [Left (1 :: Int), Right (0.5 :: Double)] of
    Just x -> toLog x ~==
      toLog ((discreteWeights !! 1) * (pdf (Continuous (Normal 0 1)) (0.5 :: Double)))
    Nothing -> False

-- Returns Nothing when not possible to compute joint density,
-- here type mismatch
check_joint_density_false =
  case jointDensity m [Right (1 :: Double), Right (0.5 :: Double)] of
    Just x -> False
    Nothing -> True
