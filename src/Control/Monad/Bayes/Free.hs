
module Control.Monad.Bayes.Free (
  FreeSampler,
  interpret,
  withRandomness,
  withPartialRandomness
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

withRandomness :: [Double] -> FreeSampler a -> a
withRandomness randomness (FreeSampler m) = run randomness m where
  run _ (Pure x) = x
  run (u:us) (Free (Random f)) = run us (f u)
  run [] (Free _) = error "FreeSampler: the list of randomness was too short"

withPartialRandomness :: MonadSample m => [Double] -> FreeSampler a -> m (a, [Double])
withPartialRandomness randomness (FreeSampler m) = run randomness m where
  run _ (Pure x) = pure (x, [])
  run (u:us) (Free (Random f)) = do {(x, vs) <- run us (f u); return (x, u:vs)}
  run [] (Free (Random f)) = do{v <- random; (x, vs) <- run [] (f v); return (x, v:vs)}
