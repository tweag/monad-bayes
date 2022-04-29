{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- This is heavily inspired by https://jtobin.io/giry-monad-implementation
-- but brought into the monad-bayes framework (i.e. Measure is an inference transformer)
-- It's largely for debugging other inference methods and didactic use, 
-- because brute force integration of measures is 
-- only practical for small programs


module Control.Monad.Bayes.Integrator where

import Control.Monad.Trans.Cont
    ( cont, runCont, Cont, ContT(ContT) )
import Control.Monad.Bayes.Class (MonadSample (random, bernoulli, normal, uniformD), condition, MonadCond (score))
import Statistics.Distribution (density)
import Numeric.Integration.TanhSinh
    ( trap, Result(result) )
import Control.Monad.Bayes.Weighted (runWeighted, Weighted, applyWeight)
import qualified Statistics.Distribution.Uniform as Statistics
import Numeric.Log (Log(ln, Exp))
import Data.Set (Set, fromList, elems)
import qualified Control.Foldl as Foldl
import Control.Foldl (Fold)
import Control.Applicative (Applicative(..))
import qualified Control.Monad.Bayes.Enumerator as Enumerator
import Data.Foldable (Foldable(foldl'))
import Debug.SimpleReflect


newtype Integrator a = Integrator (Cont Double a) 
  deriving newtype (Functor, Applicative, Monad)

runIntegrator :: (a -> Double) -> Integrator a -> Double
runIntegrator f (Integrator a) = runCont a f

instance MonadSample Integrator where
    random = fromDensityFunction $ density $ Statistics.uniformDistr 0 1
    bernoulli p = Integrator $ cont (\f -> p * f True + (1 -p) * f False)
    uniformD = fromMassFunction (const 1)

-- instance MonadCond Integrator where
--   score d = Integrator $ cont (\f -> f () * (ln $ exp d))

fromDensityFunction :: (Double -> Double) -> Integrator Double
fromDensityFunction d = Integrator $ cont $ \f ->
    integralWithQuadrature (\x -> f x * d x)
  where
    integralWithQuadrature = result . last . (\z -> trap z 0 1)

fromMassFunction :: Foldable f => (a -> Double) -> f a -> Integrator a
fromMassFunction f support = Integrator $ cont \g ->
    foldl' (\acc x -> acc + f x * g x) 0 support

empirical :: Foldable f => f a -> Integrator a
empirical = Integrator . cont . flip weightedAverage where

    weightedAverage :: (Foldable f, Fractional r) => (a -> r) -> f a -> r
    weightedAverage f = Foldl.fold (weightedAverageFold f)

    weightedAverageFold :: Fractional r => (a -> r) -> Fold a r
    weightedAverageFold f = Foldl.premap f averageFold

    averageFold :: Fractional a => Fold a a
    averageFold = (/) <$> Foldl.sum <*> Foldl.genericLength


expectation :: Integrator Double -> Double
expectation = runIntegrator id

variance :: Integrator Double -> Double
variance nu = runIntegrator (^ 2) nu - expectation nu ^ 2

momentGeneratingFunction :: Integrator Double -> Double -> Double
momentGeneratingFunction nu t = runIntegrator (\x -> exp (t * x)) nu

cumulantGeneratingFunction :: Integrator Double -> Double -> Double
cumulantGeneratingFunction nu = log . momentGeneratingFunction nu

cdf :: Integrator Double -> Double -> Double
cdf nu x = runIntegrator (negativeInfinity `to` x) nu

negativeInfinity :: Double
negativeInfinity = negate (1 / 0)

to :: (Num a, Ord a) => a -> a -> a -> a
to a b x
  | x >= a && x <= b = 1
  | otherwise        = 0

volume :: Integrator Double -> Double
volume = runIntegrator (const 1)

containing :: (Num a, Eq b) => [b] -> b -> a
containing xs x
  | x `elem` xs = 1
  | otherwise   = 0

instance Num a => Num (Integrator a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

probability :: Ord a => (a, a) -> Weighted Integrator a -> Double
probability (lower, upper) = runIntegrator (\(x,d) -> if x<upper && x  > lower then exp $ ln d else 0) . runWeighted



enumerate :: Ord a => Set a -> Weighted Integrator a -> Either String [(a, Double)]
enumerate ls meas =
    -- let norm = expectation $ exp . ln . snd <$> runWeighted meas
    Enumerator.empirical [(val, runIntegrator (\(x,d) ->
            if x == val
                then exp (ln d)
                else 0)
                (runWeighted meas)) | val <- elems ls]



example :: Either String [(Bool, Double)]
example = enumerate (fromList [True, False]) $ model 


model :: Weighted Integrator Bool
model = do

    x <- normal 0 1
    y <- bernoulli x
    condition (not y)
    return (x > 0)





-- example' :: Either String [(Bool, Double)]
-- example' = enumerate' (fromList [True, False]) $ model2

-- enumerate' :: Ord a => Set a -> Integrator a -> Either String [(a, Double)]
-- enumerate' ls meas =
--     -- let norm = expectation $ exp . ln . snd <$> runWeighted meas
--     Enumerator.empirical [(val, runIntegrator (\(x) ->
--             if x == val
--                 then 1
--                 else 0)
--                 ( meas)) | val <- elems ls]



-- model2 :: Integrator Bool
-- model2 = do

--     x <- normal 0 1
--     y <- bernoulli x
--     condition (not y)
--     return (x > 0)


-- TODO: function to make an integrator from a sampling functor


newtype SymbolicIntegrator a = SymbolicIntegrator (Cont Expr a) 
  deriving newtype (Functor, Applicative, Monad)

runSymbolicIntegrator :: (a -> Expr) -> SymbolicIntegrator a -> Expr
runSymbolicIntegrator f (SymbolicIntegrator a) = runCont a f

instance MonadSample SymbolicIntegrator where
    random = SymbolicIntegrator $ cont (\f -> ($ undefined) (\x -> fun "|" (f x ⊗ fun "d" (x)))) -- f x * d x) -- fromDensityFunction $ density $ Statistics.uniformDistr 0 1
--     bernoulli p = SymbolicIntegrator $ cont (\f -> p * f True + (1 -p) * f False)
--     uniformD = fromMassFunction (const 1)

-- -- instance MonadCond SymbolicIntegrator where
-- --   score d = SymbolicIntegrator $ cont (\f -> f () * (ln $ exp d))

-- fromDensityFunction :: (Double -> Double) -> SymbolicIntegrator Double
-- fromDensityFunction d = SymbolicIntegrator $ cont $ \f ->
--     integralWithQuadrature (\x -> f x * d x)
--   where
--     integralWithQuadrature = result . last . (\z -> trap z 0 1)

-- fromMassFunction :: Foldable f => (a -> Double) -> f a -> SymbolicIntegrator a
-- fromMassFunction f support = SymbolicIntegrator $ cont \g ->
--     foldl' (\acc x -> acc + f x * g x) 0 support
ex = runSymbolicIntegrator id (var . show <$> (random >> random))

foo = a