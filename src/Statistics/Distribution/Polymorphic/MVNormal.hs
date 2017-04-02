{-|
Module      : Statistics.Distribution.Polymorphic.MVNormal
Description : Multivariate normal distribution
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Statistics.Distribution.Polymorphic.MVNormal (
  MVNormal,
  dim,
  mean,
  chol_upper,
  covariance,
  precision,
  mvnormalDist,
  mvnormalPdf
) where

import Numeric.LinearAlgebra

import Numeric.LogDomain
import Statistics.Distribution.Polymorphic.Class

-- | Multivariate normal distribution.
-- Uses vector and matrix types provided by hmatrix.
-- Currently does not work with AutoDiff, since hmatrix does not.
data MVNormal = MVNormal (Vector R) (Matrix R)

-- | Number of dimensions.
dim :: MVNormal -> Int
dim d = size (mean d)

-- | Mean vector.
mean :: MVNormal -> Vector R
mean (MVNormal m _) = m

-- | Covariance matrix.
covariance :: MVNormal -> Herm R
covariance d = mTm (chol_upper d)

-- | Precision matrix.
precision :: MVNormal -> Herm R
precision d =
  let u_inv = inv (chol_upper d) in
    mTm (tr u_inv)

-- | Upper diagonal Cholesky factor for the covariance matrix.
chol_upper :: MVNormal -> Matrix R
chol_upper (MVNormal _ u) = u

-- | Create a normal distribution checking parameters and computing the Cholesky.
mvnormalDist :: Vector R -> Herm R -> MVNormal
mvnormalDist m variance = seq check d where
  check =
    let
      n = size m
      (k,l) = size $ unSym variance
    in
      if n == k && n == l then ()
        else error $ "MVNormal: Dimension mismatch - mean : " ++ show n ++
                     " variance: " ++ show (k,l) where
  d = MVNormal m (chol variance)

-- | PDF of a multivariate normal distribution with a given mean vector
-- and an upper triangular matrix from Cholesky decomposition of the covariance matrix.
-- Note that it does not perform any checks on the arguments, so supplying invalid arguments
-- may result in incomprehensible hmatrix errors.
mvnormalPdf :: Vector R -> Matrix R -> Vector R -> LogDomain R
mvnormalPdf m u x =
  fromLog $ (- 0.5) * (v <.> v) + c where
    -- this is probably not the fastest way to invert a triangular matrix,
    -- but I haven't found anything better in hmatrix
    v = (x - m) <# inv u
    c = -0.5 * log detSigma - (k / 2) * log (2 * pi)
    detSigma = let t = takeDiag u in t <.> t
    k = fromIntegral $ size m

instance Distribution MVNormal where
  type Domain MVNormal = Vector R
  type RealNum MVNormal = R

instance Parametric MVNormal where
  type Param MVNormal = (Vector R, Herm R)
  param d = (mean d, covariance d)
  distFromParam = uncurry mvnormalDist

instance Density MVNormal where
  pdf (MVNormal m u) x = if size m == size x then mvnormalPdf m u x
    else error $ "MVNormal PDF: expected x of lenght " ++ show (size m) ++ "but received x of length " ++ show (size x)
