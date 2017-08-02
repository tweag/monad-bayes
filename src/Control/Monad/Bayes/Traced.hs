{-|
Module      : Control.Monad.Bayes.Traced
Description : Distributions on execution traces
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  UndecidableInstances
 #-}

module Control.Monad.Bayes.Traced (
  Traced,
  Control.Monad.Bayes.Traced.hoist,
  Control.Monad.Bayes.Traced.marginal,
  Control.Monad.Bayes.Traced.mhStep
) where

import Data.Monoid ((<>))
import Control.Applicative (liftA2)
import Control.Monad.Trans (MonadTrans, lift)

import Numeric.LogDomain

import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Conditional as Cond
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Bayes.Augmented as Aug
import Control.Monad.Bayes.Inference.MCMC

data Traced m a = Traced (m (Trace (CustomReal m))) (Conditional (Weighted m) a)

traceDist :: Traced m a -> m (Trace (CustomReal m))
traceDist (Traced d _) = d

model :: Traced m a -> Conditional (Weighted m) a
model (Traced _ m) = m

emptyTrace :: Applicative m => m (Trace r)
emptyTrace = pure $ fromLists ([],[])

instance Functor m => Functor (Traced m) where
  fmap f (Traced d m) = Traced d (fmap f m)

instance Monad m => Applicative (Traced m) where
  pure x = Traced emptyTrace (pure x)
  (Traced df mf) <*> (Traced dx mx) = Traced (liftA2 (<>) df dx) (mf <*> mx)

instance (HasCustomReal m, Monad m) => Monad (Traced m) where
  (Traced dx mx) >>= f = Traced dy my where
    dy = do
      t <- dx
      (x,_) <- runWeighted $ unsafeConditional mx t
      t' <- traceDist $ f x
      return (t <> t')
    my = mx >>= model . f

instance HasCustomReal m => HasCustomReal (Traced m) where
  type CustomReal (Traced m) = CustomReal m

instance MonadTrans Traced where
  lift m = Traced emptyTrace (lift $ lift m)

instance (Distribution d, Monad m, Sampleable d (Conditional (Weighted m)), Sampleable d (Augmented m)) => Sampleable d (Traced m) where
  sample p = Traced (Aug.marginal $ joint $ sample p) (sample p)

instance (Monad m, Conditionable m) => Conditionable (Traced m) where
  factor w = Traced (factor w >> emptyTrace) (factor w)

instance MonadDist m => MonadDist (Traced m)
instance MonadBayes m => MonadBayes (Traced m)

hoist :: (CustomReal m ~ CustomReal n) => (forall x. m x -> n x) -> Traced m a -> Traced n a
hoist f (Traced d m) = Traced (f d) (Cond.hoist (Weighted.hoist f) m)

marginal :: (HasCustomReal m, Monad m) => Traced m a -> m a
marginal (Traced d m) = d >>= prior . unsafeConditional m

mhStep :: (MHKernel k, KernelDomain k ~ Trace (CustomReal m), MHSampler k ~ m,
           MonadDist m)
       => k -> Traced m a -> Traced m a
mhStep kernel (Traced d m) = Traced d' m where
  d' = do
    t <- d
    (t', w) <- proposeWithDensityRatio kernel t
    (_, p)  <- runWeighted $ unsafeConditional m t
    (_, p') <- runWeighted $ unsafeConditional m t'
    let ratio = min 1 (w * p' / p)
    accept <- bernoulli $ fromLogDomain ratio
    return $ if accept then t' else t
