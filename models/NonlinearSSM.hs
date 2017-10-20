module NonlinearSSM where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population (Population)
import Control.Monad.Bayes.Sequential (Sequential)
import Control.Monad.Bayes.Traced (Traced)
import Control.Monad.Bayes.Inference.PMMH

-- | A nonlinear series model from Doucet et al. (2000)
-- "On sequential Monte Carlo sampling methods" section VI.B
model :: MonadSample m
      => [Double]  -- ^ observed data
      -> Traced (Sequential (Population m)) [Double] -- ^ vector of state-observation pairs from t=1
model obs = do
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
        x' <- normal mean sigmaX
        factor $ normalPdf (sq x' / 20) sigmaY y
        simulate ys x' (x':acc)

  x0 <- latent $ normal 0 (sqrt 5)
  xs <- latent $ simulate obs x0 []
  return $ reverse xs
