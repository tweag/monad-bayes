{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- HMM from Anglican (https://bitbucket.org/probprog/anglican-white-paper)

module HMM
  -- ( values,
  --   hmm,
  --   syntheticData,
  -- )
where


import Control.Monad (replicateM, when)
import Control.Monad.Bayes.Class
import Data.Vector (fromList)
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Sampler (sampleSTfixed, SamplerIO)
import Pipes.Core
import qualified Pipes.Prelude as Pipes
import Pipes (MonadTrans(lift), (>->), each, yield, MFunctor (hoist))
import Pipes.Prelude (toListM)
import Numeric.Log
import Control.Monad.Bayes.Traced (mh)
import Control.Monad.Bayes.Weighted (prior)
import Data.Maybe
import Data.AEq ((~==))
import Control.Monad.Bayes.Enumerator (enumerateToDistribution)
import Control.Monad.Bayes.Sampler
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


-- | Equivalent model, but using pipes for simplicity

-- | Prior expressed as a stream
hmmPrior :: MonadSample m => Producer Int m b
hmmPrior = do
      x <- lift start
      yield x
      Pipes.unfoldr (fmap (Right . (\k -> (k, k))) . trans) x

-- | Observations expressed as a stream
hmmObservations :: Functor m => [a] -> Producer (Maybe a) m ()
hmmObservations dataset = each (Nothing : (Just <$> reverse dataset))

-- | Posterior expressed as a stream
hmmPosterior :: (MonadInfer m) => [Double] -> Producer Int m ()
hmmPosterior dataset = zipWithM hmmLikelihood
    hmmPrior
    (hmmObservations dataset)

  where

  hmmLikelihood :: MonadCond f => (Int, Maybe Double) -> f ()
  hmmLikelihood (l, o) = when (isJust o) (factor $ normalPdf (emissionMean l) 1 (fromJust o))

  zipWithM f p1 p2 = Pipes.zip p1 p2 >-> Pipes.chain f >-> Pipes.map fst

hmmPosteriorPredictive :: MonadSample m => [Double] -> Producer Double m ()
hmmPosteriorPredictive dataset =
  Pipes.hoist enumerateToDistribution (hmmPosterior dataset)
  >-> Pipes.mapM (\x -> normal (emissionMean x) 1)

hmmWithPipe = sampleIO $ reverse . init <$> toListM (hmmPosteriorPredictive (replicate 1000 1) >-> Pipes.take 3)