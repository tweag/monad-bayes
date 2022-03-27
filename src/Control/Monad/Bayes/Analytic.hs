{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Control.Monad.Bayes.Analytic where
import Control.Monad.Trans.Cont
    ( cont, runCont, Cont, ContT(ContT) )
import Control.Monad.Bayes.Class (MonadSample (random, normal), MonadInfer, condition)
import Statistics.Distribution (density)
import Numeric.Integration.TanhSinh
    ( everywhere, trap, Result(result), absolute )
import Control.Monad.Bayes.Weighted (runWeighted)
import qualified Statistics.Distribution.Uniform as Statistics
import Numeric.Log (Log(ln))


newtype Measure a = Measure (Cont Double a) deriving (Functor, Applicative, Monad)

runMeasure :: (a -> Double) -> Measure a -> Double
runMeasure f (Measure a) = runCont a f

instance MonadSample Measure where
    -- normal m s = fromDensityFunction $ density $ Statistics.normalDistr m s
    random = fromDensityFunction $ density $ Statistics.uniformDistr 0 1


fromDensityFunction :: (Double -> Double) -> Measure Double
fromDensityFunction d = Measure $ cont $ \f ->
    quadratureTanhSinh (\x -> f x * d x)
  where
    quadratureTanhSinh = result . absolute 1e-6 . (\z -> trap z 0 1)

model :: MonadInfer m => m Double
model = do
    x <- normal 0 1
    -- y <- bernoulli x
    condition (x > 0.7)
    return (x)

-- baz = runMeasure (\(x) -> if x then 1 else 0) $ model
-- baz = runMeasure (\(x,d) -> if x<0.7 && x  > 0.5 then exp $ ln d else 0) (runWeighted $ model)

probability (lower, upper) = runMeasure (\(x,d) -> if x<upper && x  > lower then exp $ ln d else 0) . runWeighted

baz = probability (-100,100.0) model


-- >>> baz
-- 0.0
