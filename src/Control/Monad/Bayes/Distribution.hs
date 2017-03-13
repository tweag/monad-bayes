{-|
Module      : Control.Monad.Bayes.Distributions
Description : Common probability distributions
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  TypeFamilies,
  MultiParamTypeClasses
 #-}


module Control.Monad.Bayes.Distribution (
  DomainType,
  RealNumType,
  Density,
  pdf,
  Sampleable,
  sample,
  Normal(Normal),
  normalDist,
  normal,
  Gamma(Gamma),
  gammaDist,
  gamma,
  Beta(Beta),
  betaDist,
  beta,
  Uniform(Uniform),
  uniformDist,
  uniform,
  Discrete(Discrete),
  discreteDist,
  discrete,
  CustomReal
) where

import qualified Data.Vector as V
import qualified Data.Foldable as Fold

import Control.Monad.Bayes.LogDomain hiding (beta, gamma)

type family DomainType d
type family RealNumType d

class Density d where
  pdf :: d -> DomainType d -> LogDomain (RealNumType d)

class Sampleable d m where
  sample :: d -> m (DomainType d)


data Normal r = Normal {mean :: r, stddev :: r}

normalDist :: (Ord r, Floating r) => r -> r -> Normal r
normalDist mu sigma
  | sigma <= 0 = error "Normal was given non-positive standard deviation"
  | otherwise  = Normal mu sigma

-- | PDF of a normal distribution parameterized by mean and stddev.
normalPdf :: (Ord a, Floating a) => a -> a -> a -> LogDomain a
normalPdf mu sigma x
  | sigma <= 0 = error "PDF: non-positive standard deviation in Normal"
  | otherwise  = fromLog $ (-0.5 * log (2 * pi * sq sigma)) +
                ((- sq (x - mu)) / (2 * sq sigma))
                  where
                    sq y = y ^ (2 :: Int)

type instance DomainType (Normal r) = r
type instance RealNumType (Normal r) = r

instance (Ord r, Floating r) => Density (Normal r) where
  pdf (Normal m s) = normalPdf m s

instance Sampleable (Normal r) Normal where
  sample = id

normal :: (Ord r, Floating r, Sampleable (Normal r) m) => r -> r -> m r
normal m s = sample (normalDist m s)



data Gamma r = Gamma {shape :: r, rate :: r}

gammaDist :: (Ord r, Floating r) => r -> r -> Gamma r
gammaDist a b =
  if a > 0 && b > 0
    then Gamma a b
    else error "Non-positive arguments to Gamma"

-- | PDF of gamma distribution parameterized by shape and rate.
gammaPdf :: (Ord a, NumSpec a) => a -> a -> a -> LogDomain a
gammaPdf a b x
  | x > 0     = fromLog $ a * log b + (a-1) * log x - b * x - logGamma a
  | otherwise = 0

type instance DomainType (Gamma r) = r
type instance RealNumType (Gamma r) = r

instance (Ord r, NumSpec r) => Density (Gamma r) where
  pdf (Gamma a b) = gammaPdf a b

instance Sampleable (Gamma r) Gamma where
  sample = id

gamma :: (Ord r, NumSpec r, Sampleable (Gamma r) m) => r -> r -> m r
gamma a b = sample (gammaDist a b)



data Beta r = Beta r r

betaDist :: (Ord r, Floating r) => r -> r -> Beta r
betaDist a b =
  if a > 0 && b > 0
    then Beta a b
    else error "Non-positive arguments to Beta"

-- | PDF of beta distribution.
betaPdf :: (Ord a, NumSpec a) => a -> a -> a -> LogDomain a
betaPdf a b x
   | a <= 0 || b <= 0 = error "Negative parameter to Beta"
   | x <= 0 = 0
   | x >= 1 = 0
   | otherwise = fromLog $ (a-1) * log x + (b-1) * log (1-x) - logBeta a b

type instance DomainType (Beta r) = r
type instance RealNumType (Beta r) = r

instance (Ord r, NumSpec r) => Density (Beta r) where
  pdf (Beta a b) = betaPdf a b

instance Sampleable (Beta r) Beta where
  sample = id

beta :: (Ord r, Floating r, Sampleable (Beta r) m, r ~ CustomReal m) => r -> r -> m r
beta a b = sample (betaDist a b)



data Uniform r = Uniform r r

uniformDist :: (Ord r) => r -> r -> Uniform r
uniformDist a b =
  if a < b
    then Uniform a b
    else error "Uniform was given an invalid interval"

-- | PDF of a continuous uniform distribution on an interval
uniformPdf :: (Ord a, Floating a) => a -> a -> a -> LogDomain a
uniformPdf a b x =
  if a <= x && x <= b then
    recip $ toLogDomain (b - a)
  else
    0

type instance DomainType (Uniform r) = r
type instance RealNumType (Uniform r) = r

instance (Ord r, NumSpec r) => Density (Uniform r) where
  pdf (Uniform a b) = uniformPdf a b

instance Sampleable (Uniform r) Uniform where
  sample = id

uniform :: (Ord r, Sampleable (Uniform r) m) => r -> r -> m r
uniform a b = sample (uniformDist a b)



data Discrete r k = Discrete {weights :: V.Vector r}

discreteDist :: (Foldable f, NumSpec r) => f r -> Discrete r k
discreteDist ws = Discrete $ normalize $ V.fromList $ Fold.toList ws

discretePdf :: (Ord r, Floating r, Integral k) => V.Vector r -> k -> LogDomain r
discretePdf ws k = let i = fromIntegral k in
  if i >= 0 && i < length ws
    then toLogDomain (ws V.! i)
    else 0

type instance DomainType (Discrete r k)  = k
type instance RealNumType (Discrete r k) = r

instance (Ord r, Floating r, Integral k) => Density (Discrete r k) where
  pdf (Discrete ws) = discretePdf ws

instance Sampleable (Discrete r k) (Discrete r) where
  sample = id

discrete :: (Sampleable (Discrete r k) m, Foldable f, NumSpec r, r ~ CustomReal m) => f r -> m k
discrete ws = sample (discreteDist ws)


-- | The type used to represent real numbers in a given monad.
-- In most cases this is just `Double`, but
-- it is abstracted mostly to support Automatic Differentiation.
type family CustomReal (m :: * -> *) :: *
