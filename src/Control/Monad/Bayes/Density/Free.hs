{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Control.Monad.Bayes.Density.Free
-- Description : Free monad transformer over random sampling
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- 'DensityT' is a free monad transformer over random sampling.
module Control.Monad.Bayes.Density.Free
  ( DensityT (..),
    hoist,
    interpret,
    withRandomness,
    runDensityT,
    traced,
  )
where

import Control.Monad.Bayes.Class (MonadDistribution (random))
import Control.Monad.RWS
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Free.Church (FT, MonadFree (..), hoistFT, iterT, iterTM, liftF)
import Control.Monad.Writer (WriterT (..))
import Data.Functor.Identity (Identity, runIdentity)

-- | Random sampling functor.
newtype SamF a = Random (Double -> a) deriving (Functor)

-- | Free monad transformer over random sampling.
--
-- Uses the Church-encoded version of the free monad for efficiency.
newtype DensityT m a = DensityT {getDensityT :: FT SamF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance MonadFree SamF (DensityT m) where
  wrap = DensityT . wrap . fmap getDensityT

instance (Monad m) => MonadDistribution (DensityT m) where
  random = DensityT $ liftF (Random id)

-- | Hoist 'DensityT' through a monad transform.
hoist :: (Monad m, Monad n) => (forall x. m x -> n x) -> DensityT m a -> DensityT n a
hoist f (DensityT m) = DensityT (hoistFT f m)

-- | Execute random sampling in the transformed monad.
interpret :: (MonadDistribution m) => DensityT m a -> m a
interpret (DensityT m) = iterT f m
  where
    f (Random k) = random >>= k

-- | Execute computation with supplied values for random choices.
withRandomness :: (Monad m) => [Double] -> DensityT m a -> m a
withRandomness randomness (DensityT m) = evalStateT (iterTM f m) randomness
  where
    f (Random k) = do
      xs <- get
      case xs of
        [] -> error "DensityT: the list of randomness was too short"
        y : ys -> put ys >> k y

-- | Execute computation with supplied values for a subset of random choices.
-- Return the output value and a record of all random choices used, whether
-- taken as input or drawn using the transformed monad.
runDensityT :: (MonadDistribution m) => [Double] -> DensityT m a -> m (a, [Double])
runDensityT randomness (DensityT m) =
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

-- | Like 'density', but use an arbitrary sampling monad.
traced :: (MonadDistribution m) => [Double] -> DensityT Identity a -> m (a, [Double])
traced randomness m = runDensityT randomness $ hoist (return . runIdentity) m
