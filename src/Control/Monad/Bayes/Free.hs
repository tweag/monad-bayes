
module Control.Monad.Bayes.Free (
  FreeSampler,
  interpret
) where

import Control.Monad.Free

import Control.Monad.Bayes.Class

newtype SamF a = Random (Double -> a)

instance Functor SamF where
  fmap f (Random k) = Random (f . k)

newtype FreeSampler a = FreeSampler (Free SamF a)
  deriving(Functor,Applicative,Monad)

instance MonadSample FreeSampler where
  random = FreeSampler $ liftF (Random id)

interpret :: MonadSample m => FreeSampler a -> m a
interpret (FreeSampler m) = foldFree f m where
  f (Random k) = fmap k random
