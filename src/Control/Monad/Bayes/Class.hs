{-# LANGUAGE
  TupleSections,
  GADTs,
  TypeFamilies,
  FlexibleContexts
 #-}


module Control.Monad.Bayes.Class where

import qualified Data.Map as Map
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
import qualified Control.Monad.Bayes.LogDomain as Log

-- | The type used to represent real numbers.
-- It is abstracted to allow for AD.
type family CustomReal (m :: * -> *) :: *

-- | Monads for building generative probabilistic models.
-- The class does not specify any conditioning primitives.
-- For better granularity discrete and continuous distributions could be separated.
class (Monad m, Ord (CustomReal m), Log.NumSpec (CustomReal m), Typeable (CustomReal m), Real (CustomReal m)) => MonadDist m where

    {-# MINIMAL primitive | (discrete, normal, gamma, beta, uniform) #-}
    -- | Discrete distribution over first n natural numbers.
    -- | The list of weights needs not sum up to 1.
    discrete :: [CustomReal m] -> m Int
    -- | Normal distribution parameterized by mean and standard deviation.
    normal   :: CustomReal m -> CustomReal m -> m (CustomReal m)
    -- | Gamma distribution parameterized by shape and rate.
    gamma    :: CustomReal m -> CustomReal m -> m (CustomReal m)
    -- | Beta distribution.
    beta     :: CustomReal m -> CustomReal m -> m (CustomReal m)
    -- | Continuous uniform distribution on an interval
    uniform  :: CustomReal m -> CustomReal m -> m (CustomReal m)

    -- | One of `Primitive` distributions.
    primitive :: Typeable a => Primitive (CustomReal m) a -> m a
    primitive (Discrete d)  = discrete d
    primitive (Continuous (Normal  m s)) = normal m s
    primitive (Continuous (Gamma   a b)) = gamma  a b
    primitive (Continuous (Beta    a b)) = beta   a b
    primitive (Continuous (Uniform a b)) = uniform a b

    -- defaults based on primitive
    discrete ps   = primitive $ Discrete ps
    normal m s    = primitive $ Continuous (Normal  m s)
    gamma  a b    = primitive $ Continuous (Gamma   a b)
    beta   a b    = primitive $ Continuous (Beta    a b)
    uniform a b   = primitive $ Continuous (Uniform a b)

    -- | Categorical distribution, weights need not be normalized.
    categorical :: [(a,CustomReal m)] -> m a
    categorical d = do
      i <- discrete (map snd d)
      return (fst (d !! i))

    -- | Dirichlet distribution, the conjugate prior to the categorical.
    -- Weights need not be normalized.
    dirichlet :: (Fractional (CustomReal m)) => [CustomReal m] -> m [CustomReal m]
    dirichlet ws = liftM normalize $ gammas ws where
      gammas = mapM (\w -> gamma w 1)
      normalize xs = map (/ (Prelude.sum xs)) xs

    -- | Bernoulli distribution.
    bernoulli :: Num (CustomReal m) => CustomReal m -> m Bool
    bernoulli p = categorical [(True,p), (False,1-p)]
    -- | Binomial distribution. Returns the number of successes.
    binomial :: Floating (CustomReal m) => Int -> CustomReal m -> m Int
    binomial n p = categorical $ map (\k -> (k, mass k)) [0..n] where
                     mass k = (realToFrac (n `choose` k)) * (p ^ k') *
                              ((1-p) ^ (n'-k')) where
                                                  n' = fromIntegral n
                                                  k' = fromIntegral k
    multinomial :: [(a,CustomReal m)] -> Int -> m [(a,Int)]
    multinomial ps n = do
      let (xs,ws) = unzip ps
      indexes <- sequence $ replicate n $ discrete ws
      let counts = Map.toList $ Map.fromListWith (+) (zip indexes (repeat 1))
      return $ map (first (xs !!)) counts
    -- | Geometric distribution starting at 0.
    geometric :: Floating (CustomReal m) => CustomReal m -> m Int
    geometric p = categorical $ map (\k -> (k, p * q ^ (fromIntegral k))) [0..] where
                             q = 1 - p
    -- | Poisson distribution.
    poisson :: Floating (CustomReal m) => CustomReal m -> m Int
    poisson p = categorical $ map (\k -> (k, mass k)) [0..] where
                             mass k = c * (p ^ (fromIntegral k)) /
                                (realToFrac (factorial k))
                             c = exp (-p)

    -- | Uniform discrete distribution.
    uniformD :: Fractional (CustomReal m) => [a] -> m a
    uniformD xs = categorical $ map (,weight) xs where
                             weight = 1 / fromIntegral (length xs)

    -- | Exponential distribution parameterized by rate.
    exponential :: Fractional (CustomReal m) => CustomReal m -> m (CustomReal m)
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
    factor :: Log.LogDomain (CustomReal m) -> m ()

    -- | Hard conditioning on an arbitrary predicate.
    -- By default implemented in terms of `factor`.
    condition :: (Floating (CustomReal m), Ord (CustomReal m)) => Bool -> m ()
    condition b = if b then factor 1 else factor 0

    -- | Soft conditioning on a noisy value.
    -- By default implemented as a `factor` with corresponding PDF.
    observe :: (r ~ CustomReal m, Ord r, Floating r, Log.NumSpec (Log.LogDomain r))
      => Primitive r a -> a -> m ()
    observe d x = factor (pdf d x)


----------------------------------------------------------------------------
-- Instances that lift probabilistic effects to standard tranformers.
type instance CustomReal (IdentityT m) = CustomReal m

instance MonadDist m => MonadDist (IdentityT m) where
  primitive = lift . primitive

instance MonadBayes m => MonadBayes (IdentityT m) where
  factor = lift . factor


type instance CustomReal (MaybeT m) = CustomReal m

instance MonadDist m => MonadDist (MaybeT m) where
  primitive = lift . primitive

instance MonadBayes m => MonadBayes (MaybeT m) where
  factor = lift . factor


type instance CustomReal (ReaderT r m) = CustomReal m

instance MonadDist m => MonadDist (ReaderT r m) where
  primitive = lift . primitive

instance MonadBayes m => MonadBayes (ReaderT r m) where
  factor = lift . factor


type instance CustomReal (WriterT w m) = CustomReal m

instance (Monoid w, MonadDist m) => MonadDist (WriterT w m) where
  primitive = lift . primitive

instance (Monoid w, MonadBayes m) => MonadBayes (WriterT w m) where
  factor = lift . factor


type instance CustomReal (StateT s m) = CustomReal m

instance MonadDist m => MonadDist (StateT s m) where
  primitive = lift . primitive

instance MonadBayes m => MonadBayes (StateT s m) where
  factor = lift . factor


type instance CustomReal (RWST r w s m) = CustomReal m

instance (Monoid w, MonadDist m) => MonadDist (RWST r w s m) where
  primitive = lift . primitive

instance (Monoid w, MonadBayes m) => MonadBayes (RWST r w s m) where
  factor = lift . factor


type instance CustomReal (ListT m) = CustomReal m

instance MonadDist m => MonadDist (ListT m) where
  primitive = lift . primitive

instance MonadBayes m => MonadBayes (ListT m) where
  factor = lift . factor


-- ExceptT is commented out for compatibility with transformers <0.4
-- We could include it through transformers-compat if necessary
-- instance MonadDist m => MonadDist (ExceptT e m) where
--     primitive = lift . primitive
--
-- instance MonadDist m => MonadDist (ExceptT e m) where
--     factor = lift . factor


type instance CustomReal (ContT r m) = CustomReal m

instance MonadDist m => MonadDist (ContT r m) where
  primitive = lift . primitive

instance MonadBayes m => MonadBayes (ContT r m) where
  factor = lift . factor


type instance CustomReal (StateCache c m) = CustomReal m

instance MonadDist m => MonadDist (StateCache c m) where
  primitive = lift . primitive

instance MonadBayes m => MonadBayes (StateCache c m) where
  factor = lift . factor
