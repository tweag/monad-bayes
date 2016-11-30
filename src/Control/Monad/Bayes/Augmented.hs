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
  Trace,
  toTrace,
  fromTrace,
  Augmented,
  withTrace,
  trace
) where

import Control.Monad.Trans
import Control.Monad.Trans.Writer

import Control.Monad.Bayes.Primitive
import Control.Monad.Bayes.Class

-- | Trace of a probabilistic program is a collection of values for all the
-- latent random variables.
newtype Trace r = Trace ([r],[Int])
  deriving(Monoid)

-- | Package values as a trace.
toTrace :: ([r],[Int]) -> Trace r
toTrace = Trace

-- | Extract values from a trace.
fromTrace :: Trace r -> ([r],[Int])
fromTrace (Trace t) = t


-- | A transformer that includes all the latent random variables in the output.
newtype Augmented m a = Augmented (WriterT (Trace (CustomReal m)) m a)
  deriving(Functor, Applicative, Monad)

type instance CustomReal (Augmented m) = CustomReal m

instance MonadTrans Augmented where
  lift = Augmented . lift

instance MonadDist m => MonadDist (Augmented m) where
  primitive d = Augmented $ do
    x <- primitive d
    tell $ toTrace $ case d of
      Discrete   _ -> (mempty, [x])
      Continuous _ -> ([x], mempty)
    return x

instance MonadBayes m => MonadBayes (Augmented m) where
  factor = lift . factor

-- | Collect program output and its trace.
withTrace :: Augmented m a -> m (a, Trace (CustomReal m))
withTrace (Augmented m) = runWriterT m

-- | Collect only program trace.
trace :: Functor m => Augmented m a -> m (Trace (CustomReal m))
trace = fmap snd . withTrace
