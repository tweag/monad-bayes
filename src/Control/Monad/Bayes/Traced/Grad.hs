{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Static
-- Description : Distributions on execution traces of full programs
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Grad
--   ( Traced,
--     hoist,
--     marginal,
--     mhStep,
--     mh,
--   )
where

import Control.Applicative (liftA2)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Traced.Common

import Control.Monad.Bayes.Weighted (Weighted, weighted, unweighted)
import Control.Monad.Trans (MonadTrans (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)
import Prelude hiding (Real)
import Control.Monad.State
import Control.Monad.Writer
import Numeric.Log (Log (ln, Exp))
import Numeric.AD
import Numeric.AD.Internal.Reverse (Reverse (Lift), Tape)
import Data.Reflection (Reifies)
import Data.Number.Erf
import Linear (V2, _y)
import Linear.V2 (V2(..))
import Control.Lens (view)
import Math.Integrators.StormerVerlet
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Bayes.Sampler (sampler)
import qualified Control.Monad.Bayes.Weighted as Weighted

newtype Density n a = Density (State [n] a) deriving newtype (Functor, Applicative, Monad)

instance MonadState [n] (Density n) where
  get = Density get
  put = Density . put


instance CustomReal n => MonadSample (Density n) where
  type (Real (Density n)) = n
  random = do
    trace <- get
    x <- case trace of
      [] -> error "ran out of randomness in the trace: this suggests that you are running HMC on a probabilistic program with stochastic control flow. Don't do that!"
      r : xs -> put xs >> pure r
    pure x


data Traced n m a = Traced
    { model :: Weighted (Density n) a,
        traceDist :: m (Trace (Real m) a)
    }

marginal :: Monad m => Traced n m a -> m a
marginal (Traced _ d) = fmap output d
    
  
instance Monad m => Functor (Traced n m) where
    fmap f (Traced m d) = Traced (fmap f m) (fmap (fmap f) d)

instance (Monad m, CustomReal (Real m)) => Applicative (Traced n m) where
    pure x = Traced (pure x) (pure (pure x))
    (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) (liftA2 (<*>) df dx)

instance (Monad m, CustomReal (Real m)) => Monad (Traced n m) where
    (Traced mx dx) >>= f = Traced my dy
        where
        my = mx >>= model . f
        dy = dx `bind` (traceDist . f)

-- instance MonadTrans (Traced n) where
--     lift m = undefined -- Traced (lift $ lift m) (fmap pure m)

instance (MonadSample m, CustomReal n, n ~ Real m) => MonadSample (Traced n m) where
    type Real (Traced n m) = Real m
    random = Traced random (fmap singleton random)
    -- normal a b = lift $ normal a b
    normal a b = inverf <$> random

instance (MonadCond m, CustomReal (Real m), n ~ Real m) => MonadCond (Traced n m) where
    score w = Traced (score w) (score w >> pure (scored w))

instance (CustomReal n, n ~ Real m, MonadInfer m) => MonadInfer (Traced n m)

density :: Density n b -> [n] -> b
density (Density m) = evalState ( m)

tangent :: CustomReal n => (forall m . MonadInfer m => m a)  -> [n] -> [n]
tangent m = grad $ pdf m

pdf :: CustomReal c => Weighted (Density c) a -> [c] -> c
pdf m = ln . exp . snd . density (weighted m)

ex :: CustomReal n => [n] -> [n]
ex = tangent example

example :: MonadInfer m => m Bool
example = do
    x <- random
    -- condition (x > 0.5)
    -- factor (Exp $ log ((1/x) ** 3))
    return (x > 0.5)


nrml :: Floating a => a -> a -> a -> a
nrml mu sigma x = 1 / (sigma * sqrt (2 * pi)) * exp ((-0.5) * (((x - mu) / sigma) ^^ 2))

getPhasePoint :: MonadSample m => Real m -> m (Real m, Real m)
getPhasePoint q = (,q) <$> normal 0 1 -- TODO: fix to normal q 1

getPhasePoints :: MonadSample m => [Real m] -> m (V2 [Real m])
getPhasePoints x = uncurry V2 . unzip <$> Prelude.mapM getPhasePoint x

hamiltonian :: CustomReal n => ([n] -> n) -> [n] -> [n] -> n
hamiltonian potential p q = negate (log $ potential q) - Prelude.sum (log (nrml q 1 p))




hmcKernel :: (MonadSample m) =>
-- (Weighted (FreeSampler IdentityN) (Reverse s a) a) ->
    (forall n. (CustomReal n) => [n] -> n) -> [Real m] -> m [Real m]
hmcKernel potential  =
    fmap
        ( view _y .
            Prelude.foldr (.) id (Prelude.replicate 100 stepForward)
        )
        . getPhasePoints
    where
    h :: (CustomReal n) => [n] -> [n] -> n
    h = hamiltonian potential
    stepForward :: (CustomReal a, Num a) => V2 [a] -> V2 [a]
    stepForward x@(V2 p q) = stormerVerlet2H 0.1 ((grad $ h (Lift <$> p))) (grad (flip (h) (Lift <$> q))) x




-- example :: (Show n, CustomReal n, MonadSample n m) => m n [n]
-- example = (\x -> ((hmcKernel . pdf) program) x) [invSigmoid 0.5]

ex2 :: IO [Bool]
ex2 = sampler $ unweighted $ marginal $ mh 10 example

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: forall n m a . MonadSample m => Traced n m a -> Traced n m a
mhStep (Traced m d) = Traced m d'
    where
    d' = do 
        tr <- d 
        let vars = variables tr
        newVars <-  hmcKernel (pdf (unsafeCoerce m)) vars
        return (tr {variables = newVars} )

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
-- mhTrans :: MonadSample m => Weighted (Density n) a -> Trace (Real m) a -> m (Trace (Real m) a)
-- mhTrans m t@Trace {variables = us, probDensity = p} = do
--   let n = length us
--   us' <- do
--     i <- undefined -- discrete $ discreteUniformAB 0 (n - 1)
--     u' <- random
--     case splitAt i us of
--       (xs, _ : ys) -> return $ xs ++ (u' : ys)
--       _ -> error "impossible"
--   ((b, q), vs) <- runWriterT $ weighted $ Weighted.hoist (WriterT . density us') m
--   let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
--   accept <- bernoulli ratio
--   return $ if accept then Trace vs b q else t

mh :: MonadSample m => Int -> Traced n m a -> m [a]
mh n (Traced m d) = fmap (map output . NE.toList) (f n)
    where
    f k
        | k <= 0 = fmap (:| []) d
        | otherwise = do
        (x :| xs) <- f (k - 1)
        y <- (_variables (hmcKernel (pdf (unsafeCoerce m)))) x
        return (y :| x : xs)


instance (Num a) => Num ([a]) where
    ([a]) + (b) = fmap ((+) a) b
    (a) + ([b]) = fmap ((+) b) a
    (a) + (b) = zipWith (+) a b

    -- (a) * (b) =  zipWith (*) (traceIt "left" a) (traceIt "right" b)
    ([a]) * (b) = fmap ((*) a) b
    (a) * ([b]) = fmap ((*) b) a
    (a) * (b) = zipWith (*) a b
    abs (a) = abs <$> a
    negate (a) = negate <$> a
    signum (a) = signum <$> a
    fromInteger a = [fromInteger a]

instance (Fractional a) => Fractional ([a]) where
    fromRational a = [fromRational a]
    recip = fmap recip

instance Floating a => Floating ([a]) where
    pi = [pi]
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    cosh = fmap cosh
    sinh = fmap sinh
    exp = fmap exp
    atan = fmap atan
    asin = fmap asin
    acosh = fmap acosh
    atanh = fmap atanh
    asinh = fmap asinh
    acos = fmap acos
    tan = fmap tan