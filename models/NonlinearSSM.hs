{-# LANGUAGE DeriveGeneric #-}

module NonlinearSSM where

import Control.Monad.Bayes.Class

import qualified Numeric.LinearAlgebra.HMatrix as LA
import Numeric.LinearAlgebra.HMatrix ((!), (><))
import Control.Monad
import Data.Vector.Storable (Storable, iterateNM, Vector)

import Foreign.Storable
import Foreign.Storable.Generic
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Generics.Deriving

param :: MonadSample m => m (Double, Double)
param = do
  let a = 0.01
  let b = 0.01
  precX <- gamma a b
  let sigmaX = 1 / sqrt precX
  precY <- gamma a b
  let sigmaY = 1 / sqrt precY
  return (sigmaX, sigmaY)

mean :: Double -> Int -> Double
mean x n = 0.5 * x + 25 * x / (1 + x * x) + 8 * cos (1.2 * fromIntegral n)

-- | A nonlinear series model from Doucet et al. (2000)
-- "On sequential Monte Carlo sampling methods" section VI.B
model ::
  (MonadInfer m) =>
  -- | observed data
  [Double] ->
  -- | prior on the parameters
  (Double, Double) ->
  -- | list of latent states from t=1
  m [Double]
model obs (sigmaX, sigmaY) = do
  let sq x = x * x
      simulate [] _ acc = return acc
      simulate (y : ys) x acc = do
        let n = length acc
        x' <- normal (mean x n) sigmaX
        factor $ normalPdf (sq x' / 20) sigmaY y
        simulate ys x' (x' : acc)
  x0 <- normal 0 (sqrt 5)
  xs <- simulate obs x0 []
  return $ reverse xs

generateData ::
  MonadSample m =>
  -- | T
  Int ->
  -- | list of latent and observable states from t=1
  m [(Double, Double)]
generateData t = do
  (sigmaX, sigmaY) <- param
  let sq x = x * x
      simulate 0 _ acc = return acc
      simulate k x acc = do
        let n = length acc
        x' <- normal (mean x n) sigmaX
        y' <- normal (sq x' / 20) sigmaY
        simulate (k -1) x' ((x', y') : acc)
  x0 <- normal 0 (sqrt 5)
  xys <- simulate t x0 []
  return $ reverse xys

g, deltaT :: Double
deltaT = 0.01
g  = 9.81

qc1 :: Double
qc1 = 0.0001

bigQL :: [[Double]]
bigQL = [[qc1 * deltaT^3 / 3, qc1 * deltaT^2 / 2],
         [qc1 * deltaT^2/ 2,  qc1 * deltaT      ]]

bigQH :: LA.Herm Double
bigQH = LA.sym $ (2 >< 2) (concat bigQL)

bigRL :: [[Double]]
bigRL = [[0.0001]]

bigRH :: LA.Herm Double
bigRH = LA.sym $ (1><1) (concat bigRL)

data StateObs = StateObs !Double !Double !Double
  deriving (Read, Show, Generic)

instance GStorable StateObs

oneStep :: MonadSample m => StateObs -> m StateObs
oneStep (StateObs x1Prev x2Prev _) = do
  eta <- mvnormal (LA.vector [0.0, 0.0]) bigQH
  let x1New = x1Prev + x2Prev * deltaT + eta ! 0
      x2New = x2Prev - g * sin x1Prev * deltaT + eta ! 1
  epsilon <- mvnormal (LA.vector [0.0]) bigRH
  let y = sin x1New + (epsilon ! 0)
  return (StateObs x1New x2New y)

bigT :: Int
bigT = 500

generateData'' :: MonadSample m => m (Vector StateObs)
generateData'' = iterateNM bigT oneStep (StateObs 0.01 0.00 (sin 0.01 + 0.0))
