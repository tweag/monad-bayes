{-# LANGUAGE
 FlexibleContexts,
 TypeFamilies
 #-}

module Main where

import Data.Vector (Vector, fromList, foldM, postscanl)
import qualified Data.Vector as Vector

import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Prior

import Plotting

main = return ()

type Mean = Double
type StdDev = Double
type Linear = (Double,Double)

data LGSSParam = LGSSParam
  {
  p0 :: (Mean, StdDev), -- ^ initial state X0 prior
  a :: Double, -- ^ transition model slope
  b :: Double, -- ^ transition model intercept
  sdX :: StdDev, -- ^ transition model noise
  c :: Double, -- ^ observation model slope
  d :: Double, -- ^ observation model intercept
  sdY :: StdDev -- ^ observation model noise
  }

-- \ One-dimensional linear Gaussian state space model.
linearGaussian :: (MonadBayes m, CustomReal m ~ Double)
               => LGSSParam
               -> Vector Double -- ^ observed sequence Y_{1:T}
               -> m (Vector Double) -- ^ latent sequence posterior X_{1:T}
linearGaussian (LGSSParam p0 a b sdX c d sdY) ys = do
  let step xs y = do{
    x' <- normal (a*(head xs) + b) sdX;
    observe (Continuous (Normal (c*x' + d) sdY)) y;
    return (x':xs)}

  x0 <- uncurry normal p0
  ps <- foldM step [x0] ys
  return $ fromList $ tail $ reverse ps

-- | One-dimensional random walk with Gaussian diffusion.
randomWalk :: (MonadBayes m, CustomReal m ~ Double)
           => (Mean, StdDev) -- ^ initial state X0 prior
           -> StdDev -- ^ transition model noise
           -> StdDev -- ^ observation model noise
           -> Vector Double -- ^ observed sequence Y_{1:T}
           -> m (Vector Double)
randomWalk p0 sdX sdY ys = linearGaussian (LGSSParam p0 1 0 sdX 1 0 sdY) ys

-- | Generate observed sequence from the prior.
generateData :: (MonadDist m, CustomReal m ~ Double)
             => LGSSParam
             -> Int -- ^ T - length of producted vector
             -> m (Vector Double) -- ^ data Y_{1:T} generated from the prior
generateData param t = do
  xs <- prior $ linearGaussian param $ Vector.replicate t undefined
  Vector.mapM (\x -> normal (c param * x + d param) (sdY param)) xs

-- | Kalman filter computing exact filtering distribution X_t|Y{1_t} for each t.
kalman :: LGSSParam
       -> Vector Double
       -> Vector (Mean, StdDev)
kalman (LGSSParam (m0,sd0) a b sdX c d sdY) ys =
  postscanl step (m0, var0) ys where
    var0 = sd0 * sd0
    varX = sdX * sdX
    varY = sdY * sdY
    step (m,s) y = (m'',s'') where
      -- TODO: check the math here
      m' = a*m + b
      s' = a*a*s + varX
      v = y - c*m' - d
      h = c*c*s' + varY
      k = c*s' / h
      m'' = m' + k*v
      s'' = s' - k*k*h
