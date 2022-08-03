{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}

    
    
    

module Control.Monad.Bayes.Inference.Lazy.MH where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sampler.Lazy
import Control.Monad.Bayes.Weighted (Weighted, weighted, unweighted)
import Control.Monad.Extra (iterateM)
import Control.Monad.State.Lazy (MonadState (get, put), State, runState)
import Numeric.Log
import System.Random
import System.Random qualified as R
import qualified Control.Monad.Bayes.Sampler.Lazy as Lazy
import qualified Control.Monad.Bayes.Population as P
import Control.Monad.Bayes.Traced (marginal)
import Control.Monad.Bayes.Sequential.Coroutine (sequentially, sis, finish)
import Control.Arrow
import Control.Monad.List (ListT(..), ap, MonadTrans (lift), MonadIO (liftIO), replicateM)
import Control.Monad.Bayes.Inference.SMC (smc, SMCConfig (SMCConfig, numSteps))
import qualified Control.Monad.Bayes.Sequential.Coroutine as Seq
import Control.Monad.Bayes.Sampler.Strict (sampleIO, SamplerIO)
import Debug.Trace (trace)
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Pipes ((>->))
import qualified Pipes as Pr
import Control.Monad.Bayes.Enumerator

mh :: forall a. Double -> Weighted Sampler a -> IO [(a, Log Double)]
mh p m = do
  -- Top level: produce a stream of samples.
  -- Split the random number generator in two
  -- One part is used as the first seed for the simulation,
  -- and one part is used for the randomness in the MH algorithm.
  g <- newStdGen >> getStdGen
  let (g1, g2) = split g
  let t = randomTree g1
  let (x, w) = runSampler (weighted m) t
  -- Now run step over and over to get a stream of (tree,result,weight)s.
  let (samples, _) = runState (iterateM step (t, x, w)) g2
  -- The stream of seeds is used to produce a stream of result/weight pairs.
  return $ map (\(_, x, w) -> (x, w)) samples
--   where
    {- NB There are three kinds of randomness in the step function.
    1. The start tree 't', which is the source of randomness for simulating the
    program m to start with. This is sort-of the point in the "state space".
    2. The randomness needed to propose a new tree ('g1')
    3. The randomness needed to decide whether to accept or reject that ('g2')
    The tree t is an argument and result,
    but we use a state monad ('get'/'put') to deal with the other randomness '(g,g1,g2)' -}
    where
    -- step :: RandomGen g => (Tree, a, Log Double) -> State g (Tree, a, Log Double)
    step (t, x, w) = do
      -- Randomly change some sites
      g <- get
      let (g1, g2) = split g
      let t' = mutateTree p g1 t
      -- Rerun the model with the new tree, to get a new
      -- weight w'.
      let (x', w') = runSampler (weighted m) t'
      -- MH acceptance ratio. This is the probability of either
      -- returning the new seed or the old one.
      let ratio = w' / w
      let (r, g2') = R.random g2
      put g2'
      if r < min 1 (exp $ ln ratio) -- (trace ("-- Ratio: " ++ show ratio) ratio))
        then return (t', x', w') -- trace ("---- Weight: " ++ show w') w')
        else return (t, x, w) -- trace ("---- Weight: " ++ show w) w)

-- Replace the labels of a tree randomly, with probability p
mutateTree :: forall g. RandomGen g => Double -> g -> Tree -> Tree
mutateTree p g (Tree a ts) = 
    let (a', g') = (R.random g :: (Double, g))
        (a'', g'') = R.random g'
    in Tree (if a' < p then a'' else a) (mutateTrees p g'' ts)

mutateTrees :: RandomGen g => Double -> g -> [Tree] -> [Tree]
mutateTrees p g (t : ts) = let (g1, g2) = split g in mutateTree p g1 t : mutateTrees p g2 ts
mutateTrees _ _ [] = error "empty tree"


a = take 4 <$> (Lazy.sampler $ population $ sis (resampleMultinomial . fromWeightedList . ap (take <$> poisson 1000) . population) 2
  $ (Seq.hoist (infiniteParticles) $ do
    x <- normal 0 1
    factor $ Exp x
    return x))

traceIt x = trace (show x) x

b :: (MonadSample m, MonadCond m) => m [Double]
b = do
  x <- normal 0 1
  factor 0.5
  fmap (x :) (b)
  
  -- return (repeat 1)

-- run = do 
--   x <- Lazy.sampler $ Lazy.independent $ population $ normal 0 1
--   print $ (take 2 ) $ fmap (take 2) x -- fmap (first (take 10)) x

infiniteParticles :: MonadSample m => Population m a -> Population m a
infiniteParticles = (P.fromWeightedList (return (repeat ((), 1))) >>)


-- newtype Seq m a = Seq {runSeq :: P.Producer (Log Double) m a} deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- instance MonadSample m => MonadSample (Seq m) where
--   random = lift Control.Monad.Bayes.Class.random
--   bernoulli = lift . bernoulli

-- instance Functor m => MonadCond (Seq m) where
--   score = Seq . P.yield

-- -- ex :: (MonadInfer m, MonadIO m, MonadSample m0) => m Double
-- ex :: Enumerator [Bool]
-- ex = P.runEffect $ 
--   (runSeq model >-> P.scan undefined () undefined)
--   >-> P.mapM_ (\x -> 
--       -- liftIO (print x) >> 
--       score x)

-- hoistS f = Seq . f . runSeq

-- -- model :: MonadInfer m => Seq m Double
-- model = -- hoistS (lift (spawn 10) >>) 
--   replicateM 100 do
--   x <- bernoulli 0.5
--   condition x -- (x > 0.5)
--   return x