module NonlinearSSM where

import Control.Monad.Bayes.Class

-- | A nonlinear series model from Doucet et al. (2000)
-- "On sequential Monte Carlo sampling methods" section VI.B
model :: (MonadInfer m, MonadSample n)
      => (forall x. n x -> m x) -- ^ tag for latent variables
      -> [Double]  -- ^ observed data
      -> m [Double] -- ^ list of latent states from t=1
model latent obs = do
  let a = 0.01
  let b = 0.01
  precX <- gamma a b
  let sigmaX = 1 / sqrt precX
  precY <- gamma a b
  let sigmaY = 1 / sqrt precY
  let sq x = x * x
      simulate [] _ acc = return acc
      simulate (y:ys) x acc = do
        let n = length acc
        let mean = 0.5 * x + 25 * x / (1 + sq x) +
                   8 * cos (1.2 * fromIntegral n)
        x' <- latent $ normal mean sigmaX
        factor $ normalPdf (sq x' / 20) sigmaY y
        simulate ys x' (x':acc)

  x0 <- latent $ normal 0 (sqrt 5)
  xs <- simulate obs x0 []
  return $ reverse xs

generateData :: MonadSample m
      => Int  -- ^ T
      -> m [(Double,Double)] -- ^ list of latent and observable states from t=1
generateData t = do
  let a = 0.01
  let b = 0.01
  precX <- gamma a b
  let sigmaX = 1 / sqrt precX
  precY <- gamma a b
  let sigmaY = 1 / sqrt precY
  let sq x = x * x
      simulate 0 _ acc = return acc
      simulate k x acc = do
        let n = length acc
        let mean = 0.5 * x + 25 * x / (1 + sq x) +
                   8 * cos (1.2 * fromIntegral n)
        x' <- normal mean sigmaX
        y' <- normal (sq x' / 20) sigmaY
        simulate (k-1) x' ((x',y'):acc)

  x0 <- normal 0 (sqrt 5)
  xys <- simulate t x0 []
  return $ reverse xys
