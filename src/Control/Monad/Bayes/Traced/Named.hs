{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Static
-- Description : Distributions on execution traces of full programs
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Named
  ( Traced,
    hoistT,
    marginal,
    mhStep,
    mh,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Free (FreeSampler)
-- import Control.Monad.Bayes.Traced.Common (mhTrans)
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Trans (MonadTrans (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)
import Data.Map hiding (singleton, map)
import Data.Text (Text)
import Numeric.Log

import Control.Monad.Bayes.Free as FreeSampler
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Numeric.Log (Log, ln)
import Statistics.Distribution.DiscreteUniform (discreteUniformAB)
import Control.Monad.State

-- | A tracing monad where only a subset of random choices are traced.
--
-- The random choices that are not to be traced should be lifted from the
-- transformed monad.
data Traced m a = Traced
  { model :: Weighted (FreeSampler m) a,
    traceDist :: m (ChoiceMap a)
  }

-- | Collection of random variables sampled during the program's execution.
data ChoiceMap a = ChoiceMap
  { -- | Sequence of random variables sampled during the program's execution.
    cm :: Map Text Double,
    -- |
    output :: a,
    -- | The probability of observing this particular sequence.
    density :: Log Double
  }

instance Functor ChoiceMap where
  fmap f t = t {output = f (output t)}

instance Applicative ChoiceMap where
  pure x = ChoiceMap {cm = empty, output = x, density = 1}
  tf <*> tx =
    ChoiceMap
      { cm = cm tf <> cm tx,
        output = output tf (output tx),
        density = density tf * density tx
      }

instance Monad ChoiceMap where
  t >>= f =
    let t' = f (output t)
     in t' {cm = cm t <> cm t', density = density t * density t'}

singleton :: Double -> ChoiceMap Double
singleton u = ChoiceMap {cm = empty, output = u, density = 1}

scored :: Log Double -> ChoiceMap ()
scored w = ChoiceMap {cm = empty, output = (), density = w}

bind :: Monad m => m (ChoiceMap a) -> (a -> m (ChoiceMap b)) -> m (ChoiceMap b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  return $ t2 {cm = cm t1 <> cm t2, density = density t1 * density t2}


instance Monad m => Functor (Traced m) where
  fmap f (Traced m d) = Traced (fmap f m) (fmap (fmap f) d)

instance Monad m => Applicative (Traced m) where
  pure x = Traced (pure x) (pure (pure x))
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) (liftA2 (<*>) df dx)

instance Monad m => Monad (Traced m) where
  (Traced mx dx) >>= f = Traced my dy
    where
      my = mx >>= model . f
      dy = undefined -- dx `bind` (traceDist . f)

instance MonadTrans Traced where
  lift m = Traced (lift $ lift m) (fmap pure m)

instance MonadSample m => MonadSample (Traced m) where
  random = Traced random (singleton <$> random)

instance MonadCond m => MonadCond (Traced m) where
  score w = Traced (score w) (score w >> pure (scored w))

instance MonadInfer m => MonadInfer (Traced m)

hoistT :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistT f (Traced m d) = Traced m (f d)

-- | Discard the trace and supporting infrastructure.
marginal :: Monad m => Traced m a -> m a
marginal (Traced _ d) = fmap output d

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: MonadSample m => (Map Text Double -> m (Map Text Double)) -> Traced m a -> Traced m a
mhStep prop (Traced m d) = Traced m d'
  where
    d' = d >>= undefined -- mhTrans prop m

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTrans :: MonadSample m => 
  (Map Text Double -> m (Map Text Double)) ->
  Weighted (FreeSampler (StateT Text m)) a -> ChoiceMap a -> m (ChoiceMap a)
mhTrans prop m t@ChoiceMap {cm = us, density = p} = do
  let n = length us
  us' <- prop us 
  ((b, q), vs) <- runWriterT $ runWeighted $ Weighted.hoist (WriterT .  withPartialRandomnessCM us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  return $ if accept then ChoiceMap us' b q else t


-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps.
mh :: MonadSample m => (Map Text Double -> m (Map Text Double)) -> Int -> Traced m a -> m [a]
mh prop n (Traced m d) = undefined
  
  -- fmap (map output . NE.toList) (f n)
  -- where
  --   f k
  --     | k <= 0 = fmap (:| []) d
  --     | otherwise = do
  --       (x :| xs) <- f (k -1)
  --       y <- mhTrans prop m x
  --       return (y :| x : xs)
