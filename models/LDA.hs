-- LDA model from Anglican
-- (https://bitbucket.org/probprog/anglican-white-paper)

module LDA where

import Numeric.Log
import qualified Control.Monad as List (replicateM)
import Data.Vector as V hiding (length, zip, mapM, mapM_)
import qualified Data.Map as Map

import Control.Monad.Bayes.Class

vocabluary :: [String]
vocabluary = ["bear", "wolf", "python", "prolog"]
topics :: [String]
topics = ["topic1", "topic2"]

docs :: [[String]]
docs = [
  words "bear wolf bear wolf bear wolf python wolf bear wolf",
  words "python prolog python prolog python prolog python prolog python prolog",
  words "bear wolf bear wolf bear wolf bear wolf bear wolf",
  words "python prolog python prolog python prolog python prolog python prolog",
  words "bear wolf bear python bear wolf bear wolf bear wolf"
  ]

wordDistPrior :: MonadSample m => m (Vector Double)
wordDistPrior = dirichlet $ V.replicate (length vocabluary) 1

topicDistPrior :: MonadSample m => m (Vector Double)
topicDistPrior = dirichlet $ V.replicate (length topics) 1

wordIndex :: Map.Map String Int
wordIndex = Map.fromList $ zip vocabluary [0..]

lda :: MonadInfer m => [[String]] -> m [Int]
lda docs = do
  word_dist_for_topic <- do
    ts <- mapM (const wordDistPrior) [0 .. length topics]
    return $ Map.fromList $ zip [0 .. length topics] ts

  let obs doc = do
        topic_dist <- fmap categorical topicDistPrior
        let f word = do
              topic <- topic_dist
              factor $ (Exp . log) $ (word_dist_for_topic Map.! topic) V.! (wordIndex Map.! word)
        mapM_ f doc

  mapM_ obs docs

  -- return samples since Discrete is not NFData
  mapM (categorical . snd) $ Map.toList word_dist_for_topic

syntheticData :: MonadSample m => Int -> Int -> m [[String]]
syntheticData d w = List.replicateM d (List.replicateM w syntheticWord) where
  syntheticWord = uniformD vocabluary
