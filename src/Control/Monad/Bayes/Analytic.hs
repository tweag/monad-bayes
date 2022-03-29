{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}

module Control.Monad.Bayes.Analytic where
import Control.Monad.Trans.Cont
    ( cont, runCont, Cont, ContT(ContT) )
import Control.Monad.Bayes.Class (MonadSample (random), condition)
import Statistics.Distribution (density)
import Numeric.Integration.TanhSinh
    ( trap, Result(result), absolute )
import Control.Monad.Bayes.Weighted (runWeighted, Weighted)
import qualified Statistics.Distribution.Uniform as Statistics
import Numeric.Log (Log(ln))


newtype Measure a = Measure (Cont Double a) deriving (Functor, Applicative, Monad)

runMeasure :: (a -> Double) -> Measure a -> Double
runMeasure f (Measure a) = runCont a f

instance MonadSample Measure where
    random = fromDensityFunction $ density $ Statistics.uniformDistr 0 1

fromDensityFunction :: (Double -> Double) -> Measure Double
fromDensityFunction d = Measure $ cont $ \f ->
    quadratureTanhSinh (\x -> f x * d x)
  where
    quadratureTanhSinh = result . absolute 1e-6 . (\z -> trap z 0 1)

probability :: Ord a => (a, a) -> Weighted Measure a -> Double
probability (lower, upper) = runMeasure (\(x,d) -> if x<upper && x  > lower then exp $ ln d else 0) . runWeighted

example :: Double
example = probability (0.8, 2.1) do
    x <- random
    condition (x > 0.7)
    return (x*2)

