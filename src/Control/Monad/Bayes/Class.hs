{-# LANGUAGE
  TupleSections,
  GADTs
 #-}


module Control.Monad.Bayes.Class where

import qualified Data.Map as Map
import Data.Number.LogFloat
import Numeric.SpecFunctions
import Data.Monoid
import Control.Arrow (first,second)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS hiding (tell)
import Control.Monad.Trans.List
--import Control.Monad.Trans.Except
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Memo.StateCache
import Data.Typeable

import Control.Monad.Bayes.Primitive

-- | Monads for building generative probabilistic models.
-- The class does not specify any conditioning primitives.
-- For better granularity discrete and continuous distributions could be separated.
class Monad m => MonadDist m where
    {-# MINIMAL primitive | (discrete, normal, gamma, beta, uniform) #-}
    -- | Discrete distribution over first n natural numbers.
    -- | The list of weights needs not sum up to 1.
    discrete :: [LogFloat] -> m Int
    -- | Normal distribution parameterized by mean and standard deviation.
    normal :: Double -> Double -> m Double
    -- | Gamma distribution parameterized by shape and rate.
    gamma :: Double -> Double -> m Double
    -- | Beta distribution.
    beta :: Double -> Double -> m Double
    -- | Continuous uniform distribution on an interval
    uniform :: Double -> Double -> m Double

    -- | One of `Primitive` distributions.
    primitive :: Typeable a => Primitive a -> m a
    primitive (Discrete d) = discrete d
    primitive (Normal m s) = normal m s
    primitive (Gamma  a b) = gamma  a b
    primitive (Beta   a b) = beta   a b
    primitive (Uniform a b) = uniform a b

    -- defaults based on primitive
    discrete ps   = primitive $ Discrete ps
    normal m s    = primitive $ Normal m s
    gamma  a b    = primitive $ Gamma  a b
    beta   a b    = primitive $ Beta   a b
    uniform a b   = primitive $ Uniform a b

    -- | Categorical distribution, weights need not be normalized.
    categorical :: [(a,LogFloat)] -> m a
    categorical d = do
      i <- discrete (map snd d)
      return (fst (d !! i))

    -- | Dirichlet distribution, the conjugate prior to the categorical.
    -- Weights need not be normalized.
    dirichlet :: [Double] -> m [Double]
    dirichlet ws = liftM normalize $ gammas ws where
      gammas = mapM (\w -> gamma w 1)
      normalize xs = map (/ (Prelude.sum xs)) xs

    -- | Bernoulli distribution.
    bernoulli :: LogFloat -> m Bool
    bernoulli p = categorical [(True,p), (False,1-p)]
    -- | Binomial distribution. Returns the number of successes.
    binomial :: Int -> LogFloat -> m Int
    binomial n p = categorical $ map (\k -> (k, mass k)) [0..n] where
                     mass k = logFloat (n `choose` k) * (p `pow` k') * ((1-p) `pow` (n'-k')) where
                                                  n' = fromIntegral n
                                                  k' = fromIntegral k
    multinomial :: [(a,LogFloat)] -> Int -> m [(a,Int)]
    multinomial ps n = do
      let (xs,ws) = unzip ps
      indexes <- sequence $ replicate n $ discrete ws
      let counts = Map.toList $ Map.fromListWith (+) (zip indexes (repeat 1))
      return $ map (first (xs !!)) counts
    -- | Geometric distribution starting at 0.
    geometric :: LogFloat -> m Int
    geometric p = categorical $ map (\k -> (k, p * q `pow` (fromIntegral k))) [0..] where
                             q = 1 - p
    -- | Poisson distribution.
    poisson :: LogFloat -> m Int
    poisson p = categorical $ map (\k -> (k, mass k)) [0..] where
                             mass k = c * (p `pow` (fromIntegral k)) / (factorial k)
                             factorial k = logToLogFloat (logFactorial k)
                             c = logToLogFloat (- fromLogFloat p) -- exp (-p)

    -- | Uniform discrete distribution.
    uniformD :: [a] -> m a
    uniformD xs = categorical $ map (,weight) xs where
                             weight = 1 / fromIntegral (length xs)

    -- | Exponential distribution parameterized by rate.
    exponential :: Double -> m Double
    exponential rate = gamma 1 (1 / rate)

    -- | Continuous uniform distribution.
    --uniform :: Double -> Double -> m Double
    --uniform 0 1 = beta 1 1
    --uniform a b = do
    --  r <- uniform 0 1
    --  return (a + (b-a)*r)

-- | Probability monads that allow conditioning.
-- Both soft and hard conditions are allowed.
class MonadDist m => MonadBayes m where

    -- | Conditioning with an arbitrary factor, as found in factor graphs.
    -- If possible it is preferred to write models using `condition` and `observe`.
    factor :: LogFloat -> m ()

    -- | Hard conditioning on an arbitrary predicate.
    -- By default implemented in terms of `factor`.
    condition :: Bool -> m ()
    condition b = if b then factor 1 else factor 0

    -- | Soft conditioning on a noisy value.
    -- By default implemented as a `factor` with corresponding PDF.
    observe :: Primitive a -> a -> m ()
    observe d x = factor (pdf d x)


----------------------------------------------------------------------------
-- Instances that lift probabilistic effects to standard tranformers.

instance MonadDist m => MonadDist (IdentityT m) where
    primitive = lift . primitive

instance MonadBayes m => MonadBayes (IdentityT m) where
    factor = lift . factor


instance MonadDist m => MonadDist (MaybeT m) where
    primitive = lift . primitive

instance MonadBayes m => MonadBayes (MaybeT m) where
    factor = lift . factor


instance MonadDist m => MonadDist (ReaderT r m) where
    primitive = lift . primitive

instance MonadBayes m => MonadBayes (ReaderT r m) where
    factor = lift . factor


instance (Monoid w, MonadDist m) => MonadDist (WriterT w m) where
    primitive = lift . primitive

instance (Monoid w, MonadBayes m) => MonadBayes (WriterT w m) where
    factor = lift . factor


instance MonadDist m => MonadDist (StateT s m) where
    primitive = lift . primitive

instance MonadBayes m => MonadBayes (StateT s m) where
    factor = lift . factor


instance (Monoid w, MonadDist m) => MonadDist (RWST r w s m) where
    primitive = lift . primitive

instance (Monoid w, MonadBayes m) => MonadBayes (RWST r w s m) where
    factor = lift . factor


instance MonadDist m => MonadDist (ListT m) where
    primitive = lift . primitive

instance MonadBayes m => MonadBayes (ListT m) where
    factor = lift . factor


-- ExceptT is commented out for compatibility with transformers <0.4
-- We could include it through transformers-compat if necessary
-- instance MonadDist m => MonadDist (ExceptT e m) where
--     primitive = lift . primitive
--
-- instance MonadBayes m => MonadBayes (ExceptT e m) where
--     factor = lift . factor


instance MonadDist m => MonadDist (ContT r m) where
    primitive = lift . primitive

instance MonadBayes m => MonadBayes (ContT r m) where
    factor = lift . factor


instance MonadDist m => MonadDist (StateCache c m) where
  primitive = lift . primitive

instance MonadBayes m => MonadBayes (StateCache c m) where
  factor = lift . factor
