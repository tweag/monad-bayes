{-|
Module      : Control.Monad.Bayes.MeanField
Description : Mean field variational approximation
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

-- It is safe to enable UndecidableInstances here so long as we don't write any Sampleable instances
-- where m doesn't shrink.
{-# LANGUAGE
  UndecidableInstances
 #-}

module Control.Monad.Bayes.MeanField (
  MeanFieldNormal,
  meanFieldNormal,
  hoist
) where

import Prelude hiding (map, unzip, length)

import Data.Vector hiding ((!), (++), slice)
import qualified Data.Vector as V
import Numeric.LinearAlgebra hiding (Vector, (!))
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader

import Statistics.Distribution.Polymorphic.MVNormal

import Control.Monad.Bayes.Simple hiding (Parametric)
import Control.Monad.Bayes.Parametric hiding (hoist)
import Control.Monad.Bayes.Inference.Proposal

-- | Safely extract the parameters.
(!) :: (Num r, Ord r) => Vector (r,r) -> Int -> (r,r)
v ! i = checkIndex v i `seq` checkParam (v V.! i)

-- | Safely extract many pairs of parameters.
slice :: (Num r, Ord r) => Int -> Int -> Vector (r,r) -> Vector (r,r)
slice i n v = checkIndex v (i+n) `seq` map checkParam (V.slice i n v)

-- | Check if the vector is long enough, generating a custom error message if not.
checkIndex :: Vector (r,r) -> Int -> ()
checkIndex v i =
  if i < length v then
    ()
  else
    error $ "MeanFieldNormal: parameter vector was too short - required at least " ++ show i ++ " elements but only " ++ show (length v) ++ " were supplied."

-- | Check if standard deviation parameter is positive, generating a custom error message if not.
checkParam :: (Num r, Ord r) => (r,r) -> (r,r)
checkParam (m,s) =
  if s <= 0 then
    error "MeanFieldNormal: standard deviation parameter was not positive"
  else
    (m,s)

-- | Check if all parameters in the vector were used, generating a custom error message if not.
checkIndexingFinal :: Monad m => StateT Int (ReaderT (Vector (CustomReal m, CustomReal m)) m) a -> ReaderT (Vector (CustomReal m, CustomReal m)) m a
checkIndexingFinal m = do
  (x,i) <- runStateT m 0
  params <- ask
  if i == length params then
    return x
  else
    error $ "MeanFieldNormal: parameter vector was too long - required " ++ show i ++ " elements but " ++ show (length params) ++ " were supplied."

-- | Transformer proposing from independent normal distributions for all continuous random variables.
-- The number of such random variables in the program should be fixed.
newtype MeanFieldNormal m a = MeanFieldNormal (StateT Int (ReaderT (Vector (CustomReal m, CustomReal m)) m) a)
  deriving(Functor,Applicative,Monad,MonadIO)

instance MonadTrans MeanFieldNormal where
  lift = MeanFieldNormal . lift . lift

instance HasCustomReal m => HasCustomReal (MeanFieldNormal m) where
  type CustomReal (MeanFieldNormal m)= CustomReal m

instance {-# OVERLAPPING #-} (Conditionable m, Sampleable (Normal (CustomReal m)) m, Domain d ~ CustomReal m, RealNum d ~ CustomReal m, Monad m, Density d) => Sampleable d (MeanFieldNormal m) where
  sample d = MeanFieldNormal $ do
    params <- ask
    index <- get
    let (m,s) = params ! index
    put (index + 1)
    d `proposingFrom` normalDist m s

instance {-# OVERLAPPING #-} (Sampleable MVNormal m, Conditionable m, CustomReal m ~ Double, Monad m) => Sampleable MVNormal (MeanFieldNormal m) where
  sample d = MeanFieldNormal $ do
    let n = dim d
    params <- ask
    index <- get
    let (ms, ss) = unzip $ slice index n params
    put (index + n)
    d `proposingFrom` mvnormalDist (convert ms) (trustSym $ diag $ convert $ map (^ (2 :: Int)) ss)

instance {-# OVERLAPPING #-} (Sampleable (Discrete r k) m, Monad m) => Sampleable (Discrete r k) (MeanFieldNormal m) where
  sample = lift . sample

instance (Conditionable m, Monad m) => Conditionable (MeanFieldNormal m) where
  factor = lift . factor

instance (Monad m, Conditionable m, Real (CustomReal m), NumSpec (CustomReal m), Sampleable (Normal (CustomReal m)) m,
          Sampleable (Discrete (CustomReal m) Int) m, Real (CustomReal m), NumSpec (CustomReal m))
          => MonadDist (MeanFieldNormal m)
instance (Monad m, Conditionable m, Real (CustomReal m), NumSpec (CustomReal m), Sampleable (Normal (CustomReal m)) m,
          Sampleable (Discrete (CustomReal m) Int) m, Real (CustomReal m), NumSpec (CustomReal m))
          => MonadBayes (MeanFieldNormal m)

-- | Propose each random variable from a normal distribution with given parameters.
-- Leave the discrete distributions unchanged.
-- Parameters for the variational distributions are given in the same order as random variables appear in the program.
meanFieldNormal :: Monad m => MeanFieldNormal m a -> Parametric (Vector (CustomReal m, CustomReal m)) m a
meanFieldNormal (MeanFieldNormal m) = parametric $ runReaderT $ checkIndexingFinal m

-- | Apply a transformation to the transformed monad.
hoist :: (CustomReal m ~ CustomReal n) => (forall x. m x -> n x) -> MeanFieldNormal m a -> MeanFieldNormal n a
hoist f (MeanFieldNormal m) = MeanFieldNormal $ mapStateT (mapReaderT f) m
