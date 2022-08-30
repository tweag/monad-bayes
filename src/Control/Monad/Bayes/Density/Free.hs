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
-- 'Density' is a free monad transformer over random sampling.
module Control.Monad.Bayes.Density.Free
  ( Density,
    hoist,
    interpret,
    withRandomness,
    density,
    traced,
    maxDraws,
  )
where

import Control.Monad.Bayes.Class (MonadSample (bernoulli, random))
import Control.Monad.RWS
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Free.Church (FT, MonadFree (..), cutoff, hoistFT, iterT, iterTM, liftF)
import Control.Monad.Writer (WriterT (..))
import Data.Functor.Identity (Identity, runIdentity)
import Data.Text (Text)

-- | Random sampling functor.
newtype SamF a = Random (Double -> a) deriving (Functor)

-- | includes bernoulli. Separated out because of speed concerns
data SamFB a = Random' (Double -> a) | Bernoulli Double (Bool -> a) deriving (Functor)

-- | Free monad transformer over random sampling.
--
-- Uses the Church-encoded version of the free monad for efficiency.
newtype Density m a = Density {runDensity :: FT SamF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance MonadFree SamF (Density m) where
  wrap = Density . wrap . fmap runDensity

instance Monad m => MonadSample (Density m) where
  random = Density $ liftF (Random id)

instance Monad m => MonadSample (FT SamFB m) where
  random = liftF (Random' id)
  bernoulli p = liftF (Bernoulli p id)

-- | Hoist 'Density' through a monad transform.
hoist :: (Monad m, Monad n) => (forall x. m x -> n x) -> Density m a -> Density n a
hoist f (Density m) = Density (hoistFT f m)

-- | Execute random sampling in the transformed monad.
interpret :: MonadSample m => Density m a -> m a
interpret (Density m) = iterT f m
  where
    f (Random k) = random >>= k

maxDraws :: MonadSample m => Integer -> FT SamFB m b -> m (Either Text b)
maxDraws n m = iterT alg $ maybeToRight "Max Draws Exceeded" <$> cutoff n m
  where
    maybeToRight e = \case
      Nothing -> Left e
      Just x -> Right x

    alg = \case
      Random' k -> random >>= k
      Bernoulli p k -> bernoulli p >>= k

-- | Execute computation with supplied values for random choices.
withRandomness :: Monad m => [Double] -> Density m a -> m a
withRandomness randomness (Density m) = evalStateT (iterTM f m) randomness
  where
    f (Random k) = do
      xs <- get
      case xs of
        [] -> error "Density: the list of randomness was too short"
        y : ys -> put ys >> k y

-- | Execute computation with supplied values for a subset of random choices.
-- Return the output value and a record of all random choices used, whether
-- taken as input or drawn using the transformed monad.
density :: MonadSample m => [Double] -> Density m a -> m (a, [Double])
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

-- | Like 'density', but use an arbitrary sampling monad.
traced :: MonadSample m => [Double] -> Density Identity a -> m (a, [Double])
traced randomness m = density randomness $ hoist (return . runIdentity) m
