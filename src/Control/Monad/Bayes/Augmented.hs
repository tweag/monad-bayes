{-|
Module      : Control.Monad.Bayes.Augmented
Description : Joint distribution of all random variables
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Augmented (
  Augmented,
  hoist,
  marginal,
  augmented,
  joint
) where

import Control.Monad.Trans
import Control.Monad.Trans.Writer

import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Trace hiding (hoist)

-- | A transformer that includes all the latent random variables in the output.
newtype Augmented m a = Augmented (WriterT (Trace (CustomReal m)) m a)
  deriving(Functor, Applicative, Monad)

type instance CustomReal (Augmented m) = CustomReal m

instance MonadTrans Augmented where
  lift = Augmented . lift

instance MonadDist m => MonadDist (Augmented m) where
  primitive d = Augmented $ do
    x <- primitive d
    tell $ fromLists $ case d of
      Discrete   _ -> (mempty, [x])
      Continuous _ -> ([x], mempty)
    return x

instance MonadBayes m => MonadBayes (Augmented m) where
  factor = lift . factor

-- | Applies a transformation to the inner monad.
hoist :: CustomReal m ~ CustomReal n => (forall x. m x -> n x) -> Augmented m a -> Augmented n a
hoist f (Augmented m) = Augmented $ mapWriterT f m

-- | Discard the trace.
marginal :: Monad m => Augmented m a -> m a
marginal (Augmented m) = fmap fst $ runWriterT m

-- | Collect program output and its trace.
augmented :: Monad m => Augmented m a -> Augmented m (a, Trace (CustomReal m))
augmented (Augmented m) = Augmented $ listen m

-- | Collect only program trace.
joint :: Monad m => Augmented m a -> Augmented m (Trace (CustomReal m))
joint = fmap snd . augmented
