{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a port of the implementation of LazyPPL: https://lazyppl.bitbucket.io/
module Control.Monad.Bayes.Sampler.Lazy where

import Control.Monad (ap)
import Control.Monad.Bayes.Class (MonadDistribution (random))
import Control.Monad.Bayes.Weighted (WeightedT, runWeightedT)
import Numeric.Log (Log (..))
import System.Random
  ( RandomGen (split),
    getStdGen,
    newStdGen,
  )
import System.Random qualified as R

-- | A 'Tree' is a lazy, infinitely wide and infinitely deep tree, labelled by Doubles
-- | Our source of randomness will be a Tree, populated by uniform [0,1] choices for each label.
-- | Often people just use a list or stream instead of a tree.
-- | But a tree allows us to be lazy about how far we are going all the time.
data Tree = Tree
  { currentUniform :: Double,
    lazyUniforms :: Trees
  }

-- | An infinite stream of 'Tree's.
data Trees = Trees
  { headTree :: Tree,
    tailTrees :: Trees
  }

-- | A probability distribution over a is
-- | a function 'Tree -> a'
-- | The idea is that it uses up bits of the tree as it runs
newtype SamplerT a = SamplerT {runSamplerT :: Tree -> a}
  deriving (Functor)

-- | Two key things to do with trees:
-- | Split tree splits a tree in two (bijectively)
-- | Get the label at the head of the tree and discard the rest
splitTree :: Tree -> (Tree, Tree)
splitTree (Tree r (Trees t ts)) = (t, Tree r ts)

-- | Preliminaries for the simulation methods. Generate a tree with uniform random labels. This uses 'split' to split a random seed
randomTree :: (RandomGen g) => g -> Tree
randomTree g = let (a, g') = R.random g in Tree a (randomTrees g')

randomTrees :: (RandomGen g) => g -> Trees
randomTrees g = let (g1, g2) = split g in Trees (randomTree g1) (randomTrees g2)

instance Applicative SamplerT where
  pure = SamplerT . const
  (<*>) = ap

-- | probabilities for a monad.
-- | Sequencing is done by splitting the tree
-- | and using different bits for different computations.
instance Monad SamplerT where
  return = pure
  (SamplerT m) >>= f = SamplerT \g ->
    let (g1, g2) = splitTree g
        (SamplerT m') = f (m g1)
     in m' g2

instance MonadDistribution SamplerT where
  random = SamplerT \(Tree r _) -> r

sampler :: SamplerT a -> IO a
sampler m = newStdGen *> (runSamplerT m . randomTree <$> getStdGen)

independent :: (Monad m) => m a -> m [a]
independent = sequence . repeat

-- | 'weightedsamples' runs a probability measure and gets out a stream of (result,weight) pairs
weightedsamples :: WeightedT SamplerT a -> IO [(a, Log Double)]
weightedsamples = sampler . independent . runWeightedT
