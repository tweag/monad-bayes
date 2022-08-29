{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a port of the implementation of LazyPPL: https://lazyppl.bitbucket.io/
module Control.Monad.Bayes.Sampler.Lazy where

import Control.Monad (ap, liftM)
import Control.Monad.Bayes.Class (MonadSample (random))
import Control.Monad.Bayes.Weighted (Weighted, weighted)
import Numeric.Log (Log (..))
import System.Random
  ( RandomGen (split),
    getStdGen,
    newStdGen,
  )
import qualified System.Random as R

-- | A 'Tree' is a lazy, infinitely wide and infinitely deep tree, labelled by Doubles
-- | Our source of randomness will be a Tree, populated by uniform [0,1] choices for each label.
-- | Often people just use a list or stream instead of a tree.
-- | But a tree allows us to be lazy about how far we are going all the time.
data Tree = Tree Double [Tree]

-- | A probability distribution over a is
-- | a function 'Tree -> a'
-- | The idea is that it uses up bits of the tree as it runs
newtype Sampler a = Sampler {runSampler :: Tree -> a}

-- | Two key things to do with trees:
-- | Split tree splits a tree in two (bijectively)
-- | Get the label at the head of the tree and discard the rest
splitTree :: Tree -> (Tree, Tree)
splitTree (Tree r (t : ts)) = (t, Tree r ts)
splitTree (Tree _ []) = error "empty tree"

-- | Preliminaries for the simulation methods. Generate a tree with uniform random labels. This uses 'split' to split a random seed
randomTree :: RandomGen g => g -> Tree
randomTree g = let (a, g') = R.random g in Tree a (randomTrees g')

randomTrees :: RandomGen g => g -> [Tree]
randomTrees g = let (g1, g2) = split g in randomTree g1 : randomTrees g2

instance Applicative Sampler where
  pure = Sampler . const
  (<*>) = ap

instance Functor Sampler where fmap = liftM

-- | probabilities for a monad.
-- | Sequencing is done by splitting the tree
-- | and using different bits for different computations.
instance Monad Sampler where
  return = pure
  (Sampler m) >>= f = Sampler \g ->
    let (g1, g2) = splitTree g
        (Sampler m') = f (m g1)
     in m' g2

instance MonadSample Sampler where
  random = Sampler \(Tree r _) -> r

sampler :: Sampler a -> IO a
sampler m = newStdGen *> (runSampler m . randomTree <$> getStdGen)

independent :: Monad m => m a -> m [a]
independent = sequence . repeat

weightedsamples :: Weighted Sampler a -> IO [(a, Log Double)]
weightedsamples = sampler . independent . weighted
