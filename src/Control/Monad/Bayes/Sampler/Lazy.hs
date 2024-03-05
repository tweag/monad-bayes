{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a port of the implementation of LazyPPL: https://lazyppl.bitbucket.io/
module Control.Monad.Bayes.Sampler.Lazy where

import Control.Monad (ap)
import Control.Monad.Bayes.Class (MonadDistribution (random))
import Control.Monad.Bayes.Weighted (WeightedT, runWeightedT)
import Control.Monad.IO.Class
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Trans
import Numeric.Log (Log (..))
import System.Random
  ( RandomGen (split),
    getStdGen,
    newStdGen,
  )
import System.Random qualified as R

-- | A 'Tree' is a lazy, infinitely wide and infinitely deep tree, labelled by Doubles.
--
--   Our source of randomness will be a Tree, populated by uniform [0,1] choices for each label.
--   Often people just use a list or stream instead of a tree.
--   But a tree allows us to be lazy about how far we are going all the time.
data Tree = Tree
  { currentUniform :: Double,
    lazyUniforms :: Trees
  }

-- | An infinite stream of 'Tree's.
data Trees = Trees
  { headTree :: Tree,
    tailTrees :: Trees
  }

-- | A probability distribution over @a@ is a function 'Tree -> a'.
--   The idea is that it uses up bits of the tree as it runs.
type Sampler = SamplerT Identity

runSampler :: Sampler a -> Tree -> a
runSampler = (runIdentity .) . runSamplerT

newtype SamplerT m a = SamplerT {runSamplerT :: Tree -> m a}
  deriving (Functor)

-- | Split a tree in two (bijectively).
splitTree :: Tree -> (Tree, Tree)
splitTree (Tree r (Trees t ts)) = (t, Tree r ts)

-- | Generate a tree with uniform random labels.
--
-- Preliminary for the simulation methods. This uses 'split' to split a random seed.
randomTree :: (RandomGen g) => g -> Tree
randomTree g = let (a, g') = R.random g in Tree a (randomTrees g')

randomTrees :: (RandomGen g) => g -> Trees
randomTrees g = let (g1, g2) = split g in Trees (randomTree g1) (randomTrees g2)

instance (Monad m) => Applicative (SamplerT m) where
  pure = lift . pure
  (<*>) = ap

-- | Sequencing is done by splitting the tree
--   and using different bits for different computations.
instance (Monad m) => Monad (SamplerT m) where
  return = pure
  (SamplerT m) >>= f = SamplerT \g -> do
    let (g1, g2) = splitTree g
    a <- m g1
    let SamplerT m' = f a
    m' g2

instance MonadTrans SamplerT where
  lift = SamplerT . const

instance (MonadIO m) => MonadIO (SamplerT m) where
  liftIO = lift . liftIO

-- | Sampling gets the label at the head of the tree and discards the rest.
instance (Monad m) => MonadDistribution (SamplerT m) where
  random = SamplerT \(Tree r _) -> pure r

-- | Runs a 'SamplerT' by creating a new 'StdGen'.
runSamplerTIO :: (MonadIO m) => SamplerT m a -> m a
runSamplerTIO m = liftIO newStdGen *> (runSamplerT m =<< randomTree <$> liftIO getStdGen)

-- | Draw a stream of independent samples.
independent :: (Monad m) => m a -> m [a]
independent = sequence . repeat

-- | Runs a probability measure and gets out a stream of @(result,weight)@ pairs
weightedSamples :: (MonadIO m) => WeightedT (SamplerT m) a -> m [(a, Log Double)]
weightedSamples = runSamplerTIO . sequence . repeat . runWeightedT
