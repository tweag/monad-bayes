{-# LANGUAGE GeneralizedNewtypeDeriving, 
    ScopedTypeVariables,
    RankNTypes, BangPatterns #-}
module Control.Monad.Bayes.Sampler.Lazy where

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Data.Monoid
import System.Random hiding (uniform)
import qualified System.Random as R
import Control.Monad
-- import Control.Monad.Extra
import Control.Monad.State.Lazy (State, state , put, get, runState)
import Numeric.Log

-- import Control.DeepSeq
-- import Control.Exception (evaluate)

-- import System.IO.Unsafe

-- import GHC.Exts.Heap
-- import System.Mem
-- import Unsafe.Coerce
import Data.Maybe

import qualified Data.Map as M
import qualified Data.List as L (lookup)

import Debug.Trace
import Control.Monad.Bayes.Class (MonadSample (random, normal), condition, MonadInfer)
import Control.Monad.Bayes.Weighted (Weighted, runWeighted)

{- | This file defines
    1. A monad 'Prob'
    2. the inference method 'lwis' (likelihood weighted importance sampling)
-}

    -- 3. 'mh' (Metropolis-Hastings algorithm based on lazily mutating parts of the tree at random)
    -- 3. 'mh1' (Single-site Metropolis-Hastings algorithm, akin to Wingate et al. It is only this that uses GHC.Exts.Heap and System.IO.Unsafe.)


-- | A 'Tree' is a lazy, infinitely wide and infinitely deep tree, labelled by Doubles
-- | Our source of randomness will be a Tree, populated by uniform [0,1] choices for each label.
-- | Often people would just use a list or stream instead of a tree.
-- | But a tree allows us to be lazy about how far we are going all the time.
data Tree = Tree Double [Tree]

-- | A probability distribution over a is 
-- | a function 'Tree -> a'
-- | The idea is that it uses up bits of the tree as it runs
newtype Prob a = Prob (Tree -> a)

-- | Two key things to do with trees:
-- | Split tree splits a tree in two (bijectively)
-- | Get the label at the head of the tree and discard the rest
splitTree :: Tree -> (Tree , Tree)
splitTree (Tree r (t : ts)) = (t , Tree r ts)
splitTree (Tree _ []) = error "empty tree"

instance Applicative Prob where
    pure = return
    (<*>) = ap
instance Functor Prob where fmap = liftM
-- | Probabilities for a monad.
-- | Sequencing is done by splitting the tree
-- | and using different bits for different computations.
instance Monad Prob where
  return a = Prob $ const a
  (Prob m) >>= f = Prob $ \g ->
    let (g1, g2) = splitTree g
        (Prob m') = f (m g1)
    in m' g2
instance MonadSample Prob where
    random = Prob $ \(Tree r _) -> r


-- sample :: Prob a -> Meas a
-- sample p = Meas $ lift p

{- | Preliminaries for the simulation methods. Generate a tree with uniform random labels
    This uses 'split' to split a random seed -}
randomTree :: RandomGen g => g -> Tree
randomTree g = let (a,g') = R.random g in Tree a (randomTrees g')
randomTrees :: RandomGen g => g -> [Tree]
randomTrees g = let (g1,g2) = split g in randomTree g1 : randomTrees g2

{- | 'runProb' runs a probability deterministically, given a source of randomness -}
runSampler :: Prob a -> Tree -> a
runSampler (Prob a) = a

{- | 'sample' runs a probability measure and gets out a stream of (result,weight) pairs -}
-- sample :: forall a. Weighted Prob a -> IO [(a,Log Double)]
sample :: Prob a -> IO [a]
sample m = do
    newStdGen *> (runSampler (sequence $ repeat m) . randomTree <$> getStdGen)


{- | Likelihood weighted importance sampling first draws n weighted samples,
    and then samples a stream of results from that regarded as an empirical distribution -}
lwis :: Int -> Weighted Prob a -> IO [a]
lwis n m = do
  xws <- sample $ runWeighted m
  let xws' = take n $ accumulate xws 0
  let max' = snd $ last xws'
  _ <- newStdGen
  rs <- randoms <$> getStdGen
  return $ fmap (\r -> fst $ head $ filter ((>= Exp (log r) * max') . snd) xws') rs

  where
    accumulate :: Num t => [(a, t)] -> t -> [(a, t)]
    accumulate ((x, w) : xws) a = (x, w + a) : (x, w + a) : accumulate xws (w + a)
    accumulate [] _ = []


example :: MonadInfer m => m Double
example = do
    x <- normal 0 1
    condition (x > 0.5)
    return x

run :: IO [Double]
run = take 10 <$> lwis 1000 example