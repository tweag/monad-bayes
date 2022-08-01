{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a port of the implementation of LazyPPL: https://lazyppl.bitbucket.io/
module Control.Monad.Bayes.Sampler.Lazy where

import Control.Monad ( ap, liftM )
import Control.Monad.Bayes.Class (MonadSample (random, normal))
import Control.Monad.Bayes.Weighted (Weighted, weighted)
import Numeric.Log (Log (..))
import System.Random
  ( RandomGen (split),
    getStdGen,
    newStdGen,
  )
import qualified System.Random as R
import qualified Data.Map as M
import GHC.IO (unsafePerformIO)
import Data.IORef
import Data.List (find)

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

-- | 'weightedsamples' runs a probability measure and gets out a stream of (result,weight) pairs
weightedsamples :: Weighted Sampler a -> IO [(a, Log Double)]
weightedsamples = sampler . independent . weighted

wiener :: Sampler (Double -> Double)
wiener = Sampler $ \(Tree _ gs) ->
    unsafePerformIO $ do
      ref <- newIORef M.empty
      modifyIORef' ref (M.insert 0 0)
      return \x -> unsafePerformIO do
        table <- readIORef ref
        case M.lookup x table of
          Just y -> return y
          Nothing -> do 
            let lower = do l <- findMaxLower x (M.keys table)
                           v <- M.lookup l table
                           return (l,v) 
            let upper = do 
                        u <- find (> x) (M.keys table)
                        v <- M.lookup u table
                        return (u,v) 
            let m = bridge lower x upper
            let y = runSampler m (gs !! (1 + M.size table))
            modifyIORef' ref (M.insert x y)
            return y

  where
    
  bridge :: Maybe (Double,Double) -> Double -> Maybe (Double,Double) -> Sampler Double
  -- not needed since the table is always initialized with (0, 0)
  -- bridge Nothing y Nothing = if y==0 then return 0 else normal 0 (sqrt y) 
  bridge (Just (x,x')) y Nothing = normal x' (sqrt (y-x))
  bridge Nothing y (Just (z,z')) = normal z' (sqrt (z-y))
  bridge (Just (x,x')) y (Just (z,z')) = normal (x' + ((y-x)*(z'-x')/(z-x))) (sqrt ((z-y)*(y-x)/(z-x)))
  bridge _ _ _ = error "undefined behavior for bridge"

  findMaxLower :: Double -> [Double] -> Maybe Double 
  findMaxLower _ [] = Nothing
  findMaxLower d (x:xs) =
    case findMaxLower d xs of 
        Nothing -> if x < d then Just x else Nothing 
        Just m -> if x > m && x < d then Just x else Just m 
