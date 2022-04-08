{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

-- |
-- This is heavily inspired by https://jtobin.io/giry-monad-implementation
-- but brought into the monad-bayes framework (i.e. Measure is an inference transformer)
-- It's largely for debugging other inference methods and didactic use, 
-- because brute force integration of measures is 
-- only practical for small programs


module Control.Monad.Bayes.Analytic where
import Control.Monad.Trans.Cont
    ( cont, runCont, Cont, ContT(ContT) )
import Control.Monad.Bayes.Class (MonadSample (random, bernoulli, normal, uniformD, categorical, poisson, beta), condition, MonadInfer, factor)
import Statistics.Distribution (density)
import Numeric.Integration.TanhSinh
    ( trap, Result(result), absolute )
import Control.Monad.Bayes.Weighted (runWeighted, Weighted, prior)
import qualified Statistics.Distribution.Uniform as Statistics
import Numeric.Log (Log(ln, Exp))
import Data.Set (Set, fromList, elems)
import qualified Control.Monad.Bayes.Enumerator
import Control.Monad (replicateM, when)
import qualified Data.Vector as VV
import Control.Monad.Error (MonadError(throwError))
import Control.Monad.Except (runExcept)
import qualified Control.Monad.Bayes.Enumerator as Enumerator
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Bayes.Traced (mh)


newtype Measure a = Measure (Cont Double a) deriving (Functor, Applicative, Monad)

runMeasure :: (a -> Double) -> Measure a -> Double
runMeasure f (Measure a) = runCont a f

instance MonadSample Measure where
    random = fromDensityFunction $ density $ Statistics.uniformDistr 0 1
    bernoulli p = Measure $ cont (\f -> p * f True + (1 -p) * f False)

fromDensityFunction :: (Double -> Double) -> Measure Double
fromDensityFunction d = Measure $ cont $ \f ->
    quadratureTanhSinh (\x -> f x * d x)
  where
    quadratureTanhSinh = result . absolute 1e-6 . (\z -> trap z 0 1)

probability :: Ord a => (a, a) -> Weighted Measure a -> Double
probability (lower, upper) = runMeasure (\(x,d) -> if x<upper && x  > lower then exp $ ln d else 0) . runWeighted


-- enumerate :: Eq a => Set a -> Weighted Measure a -> [(a, Double)]
enumerate :: Ord a => Set a -> Weighted Measure a -> Either String [(a, Double)]
enumerate ls meas = empirical [(val, runMeasure (\(x,d) -> if x == val then exp $ ln d else 0) (runWeighted meas)) | val <- elems ls]




example :: Double
example = probability (0.8, 2.1) do
    x <- random
    condition (x > 0.7)
    return (x*2)

-- example2 :: Double
example2 = enumerate (fromList [True, False]) do
    x <- bernoulli 0.7
    condition $ not x
    return x

example3 = enumerate (fromList [True, False]) $ do

    x <- normal 0 1
    y <- bernoulli x
    condition (not y)
    return (x > 0)

example4 = enumerate (fromList [True, False]) $ do

    x <- uniformD [0 :: Int,1]
    return (x > 0)

-- | The empirical distribution of a set of weighted samples
empirical :: Ord a => [(a, Double)] -> Either String [(a, Double)]
empirical samples = runExcept $ do
    let (support, probs) = unzip samples
    when (any (<= 0) probs) (throwError "Probabilities are not all strictly positive")
    return $ Enumerator.enumerate do
      i <- categorical $ VV.fromList probs
      return $ support !! i

ex :: MonadSample m => m Bool
ex = do
    y <- beta 1 1
    z <- beta 1 1
    x <- bernoulli (y*z)
    return x

ex' :: MonadInfer m => m Bool
ex' = do
    y <- beta 1 1
    factor (log $ Exp y)
    bernoulli y

ex1 = enumerate (fromList [True,False]) ex

ex2 = sampleIO $ (fmap (empirical . fmap (,1.0) . take 1000)) $ prior $ mh 10000 ex

ex1' = enumerate (fromList [True,False]) ex'

ex2' = sampleIO $ (fmap (empirical . fmap (,1.0) . take 1000)) $ prior $ mh 10000 ex'