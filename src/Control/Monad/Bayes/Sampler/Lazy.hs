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
import Control.Comonad.Cofree (Cofree ((:<)))
import qualified Data.Map
import Data.Map (keys, size, Map)
import Data.List (find)
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.State (evalStateT, modify, gets)

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
-- data Node a = Branches [a]

-- type CTree = Cofree [] Double
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

-- | Monadic equivalent to 'iterate'.  Note that it will not terminate, but may
--   still be useful in the main event loop of a program, for example.
iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f x = do
    x' <- f x
    (x':) <$> iterateM f x'

mh :: forall a. Double -> Weighted Prob a -> IO [(a, Log Double)]
mh p m = do
    -- Top level: produce a stream of samples.
    -- Split the random number generator in two
    -- One part is used as the first seed for the simulation,
    -- and one part is used for the randomness in the MH algorithm.
    _ <- newStdGen
    g <- getStdGen
    let (g1,g2) = split g
    let t = randomTree g1
    let (x, w) = runSampler (runWeighted m) t
    -- Now run step over and over to get a stream of (tree,result,weight)s.
    let (samples,_) = runState (iterateM step (t,x,w)) g2
    -- The stream of seeds is used to produce a stream of result/weight pairs.
    return $ map (\(_,x,w) -> (x,w)) samples
    {- NB There are three kinds of randomness in the step function.
    1. The start tree 't', which is the source of randomness for simulating the
    program m to start with. This is sort-of the point in the "state space".
    2. The randomness needed to propose a new tree ('g1')
    3. The randomness needed to decide whether to accept or reject that ('g2')
    The tree t is an argument and result,
    but we use a state monad ('get'/'put') to deal with the other randomness '(g,g1,g2)' -}
    where step :: RandomGen g => (Tree,a, Log Double) -> State g (Tree,a, Log Double)
          step (t, x, w) = do
            -- Randomly change some sites
            g <- get
            let (g1, g2) = split g
            let t' = mutateTree p g1 t
            -- Rerun the model with the new tree, to get a new
            -- weight w'.
            let (x', w') = runSampler (runWeighted m) t'
            -- MH acceptance ratio. This is the probability of either
            -- returning the new seed or the old one.
            let ratio =  w' /  w
            let (r, g2') = R.random g2
            put g2'
            if r < min 1 (exp $ ln ratio) -- (trace ("-- Ratio: " ++ show ratio) ratio))
              then return (t', x', w') -- trace ("---- Weight: " ++ show w') w')
              else return (t, x, w) -- trace ("---- Weight: " ++ show w) w)

-- | Replace the labels of a tree randomly, with probability p
mutateTree :: forall g. RandomGen g => Double -> g -> Tree -> Tree
mutateTree p g (Tree a ts) =
  let (a',g') = (R.random g :: (Double,g)) in
  let (a'',g'') = R.random g' in
  Tree (if a'<p then a'' else a) (mutateTrees p g'' ts)

mutateTrees :: RandomGen g => Double -> g -> [Tree] -> [Tree]
mutateTrees p g (t:ts) = let (g1,g2) = split g in mutateTree p g1 t : mutateTrees p g2 ts
mutateTrees _ _ [] = error "empty tree"


example :: MonadInfer m => m Double
example = do
    x <- normal 0 1
    condition (x > 0.5)
    return x

-- run :: IO [Double]
run :: IO [(Double, Log Double)]
run = take 10 <$> mh 1 example

-- wiener :: Prob (Double -> State (Data.Map.Map Double Double) Double)
-- wiener = Prob $ \(Tree _ gs) x -> do
--         modify (Data.Map.insert 0 0)
--         table <- get 
--         case Data.Map.lookup x table of
--             Just y -> return y
--             Nothing -> return $ fromMaybe undefined $ do
--                         let lower = do
--                                         l <- findMaxLower x (keys table)
--                                         v <- Data.Map.lookup l table
--                                         return (l,v)
--                         let upper = do {u <- find (> x) (keys table) ;
--                                         v    <- Data.Map.lookup u table ; return (u,v) }
--                         let m = bridge lower x upper
--                         let y = runSampler m (gs !! (1 + size table))
--                         return y

--                                 --  modify (Data.Map.insert x y)


-- findMaxLower :: Double -> [Double] -> Maybe Double
-- findMaxLower d [] = Nothing
-- findMaxLower d (x:xs) = let y = findMaxLower d xs in
--                        case y of
--                            Nothing -> if x < d then Just x else Nothing
--                            Just m -> do
--                                           if x > m && x < d then Just x else Just m

-- bridge :: Maybe (Double,Double) -> Double -> Maybe (Double,Double) -> Prob Double
-- -- not needed since the table is always initialized with (0, 0)
-- -- bridge Nothing y Nothing = if y==0 then return 0 else normal 0 (sqrt y) 
-- bridge (Just (x,x')) y Nothing = normal x' (sqrt (y-x))
-- bridge Nothing y (Just (z,z')) = normal z' (sqrt (z-y))
-- bridge (Just (x,x')) y (Just (z,z')) = normal (x' + ((y-x)*(z'-x')/(z-x))) (sqrt ((z-y)*(y-x)/(z-x)))


