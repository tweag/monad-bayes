{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wall                   #-}

module EssenceGeneric (

  ) where

import Control.Applicative (liftA2)
import Control.Monad.RWS (MonadIO, MonadTrans, lift)
import Control.Monad.State (evalStateT, modify)
import Control.Monad.Trans.Free.Church (FT, MonadFree (..), hoistFT, iterTM, liftF)
import Control.Monad.Writer (WriterT (..), tell)
import Control.Monad.State (StateT, runStateT, mapStateT, put, get)
import Control.Monad.Identity (IdentityT)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Reader.Class (MonadReader)

import Statistics.Distribution (ContDistr, quantile, logDensity)
import Statistics.Distribution.Uniform (uniformDistr)
import Statistics.Distribution.Normal (normalDistr)
import Numeric.Log (Log(..))
import System.Random.Stateful (StatefulGen, mkStdGen, newIOGenM, uniformDouble01M, uniformRM, split)
import qualified System.Random.MWC.Distributions as MWC

import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)

import qualified Data.Random as R
import System.Random.Stateful (setStdGen, newStdGen)

import Prelude hiding (Real)
import Data.Kind (Type)

import qualified Debug.Trace as D


type CustomReal a = RealFloat a

class (CustomReal (Real m), Monad m) => MonadDistribution m where
  type Real m :: Type
  random :: m (Real m)
  bernoulli :: Real m -> m Bool
  bernoulli p = fmap (< p) random

instance MonadDistribution m => MonadDistribution (StateT s m) where
  type Real (StateT s m) = Real m
  random = lift random
  bernoulli = lift . bernoulli

instance (Monoid w, MonadDistribution m) => MonadDistribution (WriterT w m) where
  type Real (WriterT w m) = Real m
  random = lift random
  bernoulli = lift . bernoulli

geometric :: MonadDistribution m => m Int
geometric = do
  x <- random
  if x < 0.2
    then return 1
    else do y <- geometric
            return $ 1 + y

-- | Random sampling functor.
newtype SamF n a = Random (n -> a)

instance Functor (SamF n) where
  fmap f (Random k) = Random (f . k)

-- | Free monad transformer over random sampling.
--
-- Uses the Church-encoded version of the free monad for efficiency.
newtype Density m a = Density {runDensity :: FT (SamF (Real m)) m a}
  deriving newtype (Functor, Applicative, Monad)

instance (Real m ~ n, CustomReal (n)) => MonadFree (SamF n) (Density m) where
  wrap = Density . wrap . fmap runDensity

instance (Monad m, CustomReal (Real m)) => MonadDistribution (Density m) where
  type Real (Density m) = Real m
  random = Density $ liftF (Random id)

-- | Hoist 'Density' through a monad transform.
hoistD :: (Real m ~ Real n, Monad m, Monad n) =>
          (forall x. m x -> n x) -> Density m a -> Density n a
hoistD f (Density m) = Density (hoistFT f m)

-- | Execute computation with supplied values for a subset of random choices.
-- Return the output value and a record of all random choices used, whether
-- taken as input or drawn using the transformed monad.
density :: MonadDistribution m => [Real m] -> Density m a -> m (a, [Real m])
density randomness (Density m) =
  runWriterT $ evalStateT (iterTM f $ hoistFT lift m) randomness
  where
    f (Random k) = do
      -- This block runs in StateT [Double] (WriterT [Double]) m.
      -- StateT propagates consumed randomness while WriterT records
      -- randomness used, whether old or new.
      xs <- get
      x <- case xs of
        [] -> random
        y : ys -> put ys >> return y
      tell [x]
      k x
