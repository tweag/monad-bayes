{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Control.Monad.Bayes.Analytic where
import Control.Monad.Trans.Cont
import Control.Monad.Bayes.Class (MonadSample (random, normal, bernoulli), MonadCond (score), MonadInfer, condition)
import qualified Statistics.Distribution.Normal as Statistics
import Statistics.Distribution (density)
import Numeric.Integration.TanhSinh
import Control.Monad.Bayes.Weighted (runWeighted)
import qualified Statistics.Distribution.Uniform as Statistics
import Numeric.Log (Log(ln))


newtype Measure a = Measure (Cont Double a) deriving (Functor, Applicative, Monad)

instance MonadSample Measure where
    normal m s = fromDensityFunction $ density $ Statistics.normalDistr m s
    random = fromDensityFunction $ density $ Statistics.uniformDistr 0 1


fromDensityFunction :: (Double -> Double) -> Measure Double
fromDensityFunction d = Measure $ cont $ \f ->
    quadratureTanhSinh (\x -> f x * d x)
  where
    quadratureTanhSinh = result . last . everywhere trap

model :: MonadSample m => m Bool
model = do
    x <- bernoulli 0.5
    return x

-- baz = runMeasure (\x -> if x<0.0 then 1.0 else 0.0) $ normal 0 1
-- baz = runMeasure (fst) $ runWeighted model

-- >>> baz
-- 0.0
