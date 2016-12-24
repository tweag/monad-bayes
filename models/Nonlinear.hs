{-# LANGUAGE
 FlexibleContexts,
 TypeFamilies
 #-}

module Nonlinear (
  model,
  generative,
  synthesizeData,
  posterior,
  reference,
  rmse,
  averageVec
  ) where

import Prelude hiding (map, length, sum, transpose, zipWith, replicate)
import qualified Data.List as List

import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Data.Vector hiding (reverse)
import Control.Exception.Base

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Conditional
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference

-- | A nonlinear series model from Doucet et al. (2000)
-- "On sequential Monte Carlo sampling methods" section VI.B
model :: (MonadDist m, MonadTrans t, MonadDist (t m), r ~ CustomReal (t m),
          r ~ CustomReal m)
      => Int -- ^ number of time steps
      -> t m (Vector (r, r)) -- ^ vector of state-observation pairs from t=1
model t = do
  let sq x = x * x
      simulate 0 _ acc = return acc
      simulate n x acc = do
        let mean = 0.5 * x + 25 * x / (1 + sq x) +
                   8 * (cos (1.2 * fromIntegral n))
        x' <- lift $ normal mean 1
        y' <- normal (sq x' / 20) 1
        simulate (n-1) x' ((x',y'):acc)

  x0 <- lift $ normal 0 (sqrt 5)
  ps <- simulate t x0 []
  return $ fromList $ reverse ps

-- | Generative version of 'model', samples both x and y
generative :: MonadDist m => Int -> m (Vector (CustomReal m, CustomReal m))
generative = runIdentityT . model

-- | Generates synthetic data from the prior
synthesizeData :: MonadDist m => Int -> m (Vector (CustomReal m))
synthesizeData = fmap (map snd) . generative

-- | Posterior distribution over xs conditional on ys
posterior :: MonadBayes m
          => Vector (CustomReal m) -- ^ observations ys
          -> m (Vector (CustomReal m)) -- ^ latent states xs starting from t=1
posterior ys = fmap (map fst) $
  unsafeConditional (model (length ys)) (fromLists (toList ys,[]))

-- | Averages a weighted sample of vectors.
-- Assumes the weights are normalized.
averageVec :: Num r => [(Vector r, r)] -> Vector r
averageVec ps = List.foldr (zipWith (+)) zeros $
                List.map (\(v,w) -> map (* w) v) ps where
                  zeros = assert (List.all ((== k) . length . fst) ps) $
                          replicate k 0
                  k = length $ fst $ List.head ps

-- | The reference inference algorithm is SMC with 10000 particles.
reference :: MonadDist m
          => Vector (CustomReal m) -- ^ ys
          -> m (Vector (CustomReal m)) -- ^ mean of xs from all particles
reference ys = fmap averageVec $ explicitPopulation $ normalize $
               smc k n (posterior ys) where
  k = length ys
  n = 10000

-- | Root-mean-square error as given by the first formula in section VI of Doucet et al.
rmse :: Floating r
     => Vector r -- ^ reference values for the posterior mean
     -> Vector r -- ^ estimated posterior mean
     -> r
rmse ref xs = sum $ map (^ 2) $ zipWith (-) ref xs
  -- average $ map (sqrt . average) $ transpose $
  -- map (zipWith sqerr ref) xss where
  --
  --   average v | length v > 0 = sum v / fromIntegral (length v)
  --   average _                = 0
  --
  --   transpose vss = map (\n -> map (! n) vss) ns where
  --     ns = generate (length (vss ! 0)) id
  --
  --   sqerr x y = (x - y) ^ 2
