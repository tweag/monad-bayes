{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Bayes.Inference.Lazy.MH where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler.Lazy
import Control.Monad.Bayes.Weighted (Weighted, weighted)
import Control.Monad.Extra (iterateM)
import Control.Monad.State.Lazy (MonadState (get, put), State, runState)
import Numeric.Log
import System.Random
import System.Random qualified as R

-- mh :: forall a. Double -> Weighted Sampler a -> IO [(a, Log Double)]
-- mh p m = do
--   -- Top level: produce a stream of samples.
--   -- Split the random number generator in two
--   -- One part is used as the first seed for the simulation,
--   -- and one part is used for the randomness in the MH algorithm.
--   g <- newStdGen >> getStdGen
--   let (g1, g2) = split g
--   let t = randomTree g1
--   let (x, w) = runSampler (weighted m) t
--   -- Now run step over and over to get a stream of (tree,result,weight)s.
--   let (samples, _) = runState (iterateM step (t, x, w)) g2
--   -- The stream of seeds is used to produce a stream of result/weight pairs.
--   return $ map (\(_, x, w) -> (x, w)) samples
--   where
--     {- NB There are three kinds of randomness in the step function.
--     1. The start tree 't', which is the source of randomness for simulating the
--     program m to start with. This is sort-of the point in the "state space".
--     2. The randomness needed to propose a new tree ('g1')
--     3. The randomness needed to decide whether to accept or reject that ('g2')
--     The tree t is an argument and result,
--     but we use a state monad ('get'/'put') to deal with the other randomness '(g,g1,g2)' -}
--     step :: RandomGen g => (Tree, a, Log Double) -> State g (Tree, a, Log Double)
--     step (t, x, w) = do
--       -- Randomly change some sites
--       g <- get
--       let (g1, g2) = split g
--       let t' = mutateTree p g1 t
--       -- Rerun the model with the new tree, to get a new
--       -- weight w'.
--       let (x', w') = runSampler (weighted m) t'
--       -- MH acceptance ratio. This is the probability of either
--       -- returning the new seed or the old one.
--       let ratio = w' / w
--       let (r, g2') = R.random g2
--       put g2'
--       if r < min 1 (exp $ ln ratio) -- (trace ("-- Ratio: " ++ show ratio) ratio))
--         then return (t', x', w') -- trace ("---- Weight: " ++ show w') w')
--         else return (t, x, w) -- trace ("---- Weight: " ++ show w) w)

--     -- Replace the labels of a tree randomly, with probability p
--     mutateTree :: forall g. RandomGen g => Double -> g -> Tree -> Tree
--     mutateTree p g (Tree a ts) =
--       let (a', g') = (R.random g :: (Double, g))
--        in let (a'', g'') = R.random g'
--            in Tree (if a' < p then a'' else a) (mutateTrees p g'' ts)

--     mutateTrees :: RandomGen g => Double -> g -> [Tree] -> [Tree]
--     mutateTrees p g (t : ts) = let (g1, g2) = split g in mutateTree p g1 t : mutateTrees p g2 ts
--     mutateTrees _ _ [] = error "empty tree"
