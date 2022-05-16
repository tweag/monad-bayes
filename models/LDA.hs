-- LDA model from Anglican
-- (https://bitbucket.org/probprog/anglican-white-paper)

module LDA where

import qualified Control.Monad as List (replicateM)
import Control.Monad.Bayes.Class
import qualified Data.Map as Map
import qualified Data.Vector as V hiding (length, mapM, mapM_)
import Numeric.Log

vocabulary :: [String]
vocabulary = ["bear", "wolf", "python", "prolog"]

topics :: [String]
topics = ["topic1", "topic2"]

documents :: [[String]]
documents =
  [ words "bear wolf bear wolf bear wolf python wolf bear wolf",
    words "python prolog python prolog python prolog python prolog python prolog",
    words "bear wolf bear wolf bear wolf bear wolf bear wolf",
    words "python prolog python prolog python prolog python prolog python prolog",
    words "bear wolf bear python bear wolf bear wolf bear wolf"
  ]

wordDistPrior :: MonadSample m => m (V.Vector Double)
wordDistPrior = dirichlet $ V.replicate (length vocabulary) 1

topicDistPrior :: MonadSample m => m (V.Vector Double)
topicDistPrior = dirichlet $ V.replicate (length topics) 1

wordIndex :: Map.Map String Int
wordIndex = Map.fromList $ zip vocabulary [0 ..]

lda :: MonadInfer m => [[String]] -> m (Map.Map String (V.Vector (String, Double)), 
  
  [V.Vector Double])
lda docs = do
  word_dist_for_topic <- do
    ts <- List.replicateM (length topics) wordDistPrior
    return $ Map.fromList $ zip topics ts
  let obs doc = do
        topic_dist <- topicDistPrior
        let f word = do
              topic <- (fmap (topics !!) . categorical) topic_dist
              factor $ (Exp . log) $ (word_dist_for_topic Map.! topic) V.! (wordIndex Map.! word)
        mapM_ f doc
        return topic_dist
  td <- mapM obs docs
  -- return samples since Discrete is not NFData
  -- mapM (categorical . snd) $ toList
  return 
    (fmap ( V.zip (V.fromList vocabulary) ) word_dist_for_topic, 
      td)

syntheticData :: MonadSample m => Int -> Int -> m [[String]]
syntheticData d w = List.replicateM d (List.replicateM w syntheticWord)
  where
    syntheticWord = uniformD vocabulary
