{-# LANGUAGE ImportQualifiedPost #-}

-- LDA model from Anglican
-- (https://bitbucket.org/probprog/anglican-white-paper)

-- This model is just a toy/reference implementation.
-- A more serious one would not store documents as lists of words.
-- The point is just to showcase the model

module LDA where

import Control.Monad qualified as List (replicateM)
import Control.Monad.Bayes.Class
  ( MonadDistribution (categorical, dirichlet, uniformD),
    MonadMeasure,
    factor,
  )
import Control.Monad.Bayes.Sampler.Strict (sampleIOfixed)
import Control.Monad.Bayes.Traced (mh)
import Control.Monad.Bayes.Weighted (unweighted)
import Data.Map qualified as Map
import Data.Text (Text, words)
import Data.Vector as V (Vector, replicate, (!))
import Data.Vector qualified as V hiding (length, mapM, mapM_)
import Numeric.Log (Log (Exp))
import Text.Pretty.Simple (pPrint)
import Prelude hiding (words)

vocabulary :: [Text]
vocabulary = ["bear", "wolf", "python", "prolog"]

topics :: [Text]
topics = ["topic1", "topic2"]

type Documents = [[Text]]

documents :: Documents
documents =
  [ words "bear wolf bear wolf bear wolf python wolf bear wolf",
    words "python prolog python prolog python prolog python prolog python prolog",
    words "bear wolf bear wolf bear wolf bear wolf bear wolf",
    words "python prolog python prolog python prolog python prolog python prolog",
    words "bear wolf bear python bear wolf bear wolf bear wolf"
  ]

wordDistPrior :: MonadDistribution m => m (V.Vector Double)
wordDistPrior = dirichlet $ V.replicate (length vocabulary) 1

topicDistPrior :: MonadDistribution m => m (V.Vector Double)
topicDistPrior = dirichlet $ V.replicate (length topics) 1

wordIndex :: Map.Map Text Int
wordIndex = Map.fromList $ zip vocabulary [0 ..]

lda ::
  MonadMeasure m =>
  Documents ->
  m (Map.Map Text (V.Vector (Text, Double)), [(Text, V.Vector (Text, Double))])
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
  return
    ( fmap (V.zip (V.fromList vocabulary)) word_dist_for_topic,
      zip (fmap (foldr1 (\x y -> x <> " " <> y)) docs) (fmap (V.zip $ V.fromList ["topic1", "topic2"]) td)
    )

syntheticData :: MonadDistribution m => Int -> Int -> m [[Text]]
syntheticData d w = List.replicateM d (List.replicateM w syntheticWord)
  where
    syntheticWord = uniformD vocabulary

runLDA :: IO ()
runLDA = do
  s <- sampleIOfixed $ unweighted $ mh 1000 $ lda documents
  pPrint (head s)
