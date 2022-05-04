{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module TestNamed where

import Control.Monad.Bayes.Class
    ( condition, MonadCond, MonadSample(bernoulli, uniformD, random) )
import Control.Monad.Bayes.Enumerator ()
import Control.Monad.Bayes.Inference.SMC ()
import Control.Monad.Bayes.Population ()
import Control.Monad.Bayes.Sampler ( sampleIO, sampleIOfixed )
import Data.AEq ()
import Numeric.Log ()
import Sprinkler ()
import Control.Monad.Bayes.Free
    ( withPartialRandomnessCM, FreeSampler ) 
import Control.Monad.State
    ( (>=>), StateT, MonadTrans(..), evalStateT, MonadState(put) )
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.Bayes.Weighted ( prior )
import Control.Monad.Bayes.Traced.Named
    ( mh, traced, ChoiceMap(cm), Traced(traceDist) ) 
import Lens.Micro.GHC (ix)
import Data.Map (keys)


ex :: MonadSample m => FreeSampler (StateT T.Text m)  (Bool, Bool)
ex = do
  x <- traced "x" $ bernoulli 0.5
  y <- traced "y" $ bernoulli 0.5
  _ <- bernoulli 0.5
  return (x,y)



goodProposal = (ix "xij" (const random) >=> ix "y" (const random))

-- prop2 :: Proposal
badProposal k = do
  key <- uniformD ["x", "y"]
  ix key (const random) k



ex2 :: (MonadTrans t, MonadState T.Text m, MonadSample (t m), MonadCond (t m)) =>
 t m (Double, Double)
ex2 = do
  x <-  traced "x" (random >> traced "i" (random >> traced "j" random))
  y <- traced "y" random
  condition (x > 0.7 && y > 0.7)
  _ <- bernoulli 0.5
  return (x,y)



test1 = do

  -- check that the right variables are traced
  choiceMap <- sampleIO $ prior $ flip evalStateT "" $ cm <$> traceDist ex2
  print $ keys choiceMap == ["x","xi","xij","y"]

  -- check that mh on the model succeeds with a good choicemap and always accepts
  s <- sampleIO $ prior $ flip evalStateT "" $ mh (const $ return $ M.fromList [("xij", 0.8), ("y", 0.8)]) 3 ex2
  print ([(0.8,0.8),(0.8,0.8),(0.8,0.8)] == init s)

  -- check that mh on the model always rejects with a bad choicemap
  s2 <- sampleIOfixed $ prior $ flip evalStateT "" $ mh (const $ return $ M.fromList [("xij", 0.3), ("y", 0.3)]) 3 ex2
  print ([(0.8,0.8),(0.8,0.8),(0.8,0.8)] /= init s2)
  print (head s2 == head (tail s2))


  -- check that a choicemap works
  s3 <- sampleIO $ withPartialRandomnessCM (M.fromList [("y", 0.5), ("x", 0.8)]) ex
  print (take 2 (snd s3) == [0.8,0.5])

  -- check that mh succeeds with a good proposal
  (n1,n2) <-  fmap head $ sampleIOfixed $ prior $ flip evalStateT "" $ mh
  -- prop2
          goodProposal
          1000 ex2

  print (n1 > 0.7 && n2 > 0.7)

  -- check that mh fails with a non-ergodic proposal
  (n1',n2') <-  fmap head $ sampleIOfixed $ prior $ flip evalStateT "" $ mh
          badProposal
          1000 ex2

  print (n1' < 0.7 || n2' < 0.7)

