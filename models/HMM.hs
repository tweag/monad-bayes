{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- HMM from Anglican (https://bitbucket.org/probprog/anglican-white-paper)

module HMM
  ( values,
    hmm,
    syntheticData,
  )
where

-- Hidden Markov Models
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
  ( MonadInfer,
    MonadSample (categorical, uniformD),
    factor,
    normalPdf,
  )
import Data.Vector (fromList)

-- | Observed values
values :: [Double]
values =
  [ 0.9,
    0.8,
    0.7,
    0,
    -0.025,
    -5,
    -2,
    -0.1,
    0,
    0.13,
    0.45,
    6,
    0.2,
    0.3,
    -1,
    -1
  ]

-- | The transition model.
trans :: MonadSample m => Int -> m Int
trans 0 = categorical $ fromList [0.1, 0.4, 0.5]
trans 1 = categorical $ fromList [0.2, 0.6, 0.2]
trans 2 = categorical $ fromList [0.15, 0.7, 0.15]
trans _ = error "unreachable"

-- | The emission model.
emissionMean :: Int -> Double
emissionMean 0 = -1
emissionMean 1 = 1
emissionMean 2 = 0
emissionMean _ = error "unreachable"

-- | Initial state distribution
start :: MonadSample m => m Int
start = uniformD [0, 1, 2]

-- | Example HMM from http://dl.acm.org/citation.cfm?id=2804317
hmm :: (MonadInfer m) => [Double] -> m [Int]
hmm dataset = f dataset (const . return)
  where
    expand x y = do
      x' <- trans x
      factor $ normalPdf (emissionMean x') 1 y
      return x'
    f [] k = start >>= k []
    f (y : ys) k = f ys (\xs x -> expand x y >>= k (x : xs))

syntheticData :: MonadSample m => Int -> m [Double]
syntheticData n = replicateM n syntheticPoint
  where
    syntheticPoint = uniformD [0, 1, 2]
