-- LDA model from Anglican
-- (https://bitbucket.org/probprog/anglican-white-paper)

module LDA where

import qualified Data.Map as Map

import Control.Monad.Bayes.Simple

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

word_dist_prior :: (MonadBayes m, CustomReal m ~ Double) => m [Double]
word_dist_prior = dirichlet $ map (const 1) vocabluary

topic_dist_prior :: (MonadBayes m, CustomReal m ~ Double) => m [Double]
topic_dist_prior = dirichlet $ map (const 1) topics

word_index :: Map.Map String Int
word_index = Map.fromList $ zip vocabluary [0..]

lda :: (MonadBayes m, CustomReal m ~ Double) => m (Map.Map Int (Discrete Double))
lda = do
  word_dist_for_topic <- do
    ts <- mapM (const $ fmap discreteDist word_dist_prior) [0 .. length topics]
    return $ Map.fromList $ zip [0 .. length topics] ts

  let obs doc = do
        topic_dist <- fmap discreteDist topic_dist_prior
        let f word = do
              topic <- sample topic_dist
              observe (word_dist_for_topic Map.! topic) (word_index Map.! word)
        mapM_ f doc

  mapM_ obs docs

  return word_dist_for_topic
