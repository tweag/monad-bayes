{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- HMM from Anglican (https://bitbucket.org/probprog/anglican-white-paper)

module HMM
  ( values,
    hmm,
    syntheticData,
  )
where

--Hidden Markov Models

--Hidden Markov Models
import Control.Monad (replicateM, when)
import Control.Monad.Bayes.Class
import Data.Vector (fromList)
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Sampler (sampleSTfixed, SamplerIO)
import Pipes.Core
import qualified Pipes.Prelude as Pipes
import Pipes (MonadTrans(lift), (>->), each, yield)
import Pipes.Prelude (toListM)
import Numeric.Log
import Control.Monad.Bayes.Traced (mh)
import Control.Monad.Bayes.Weighted (prior)
import Data.Maybe
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

forward :: MonadSample m => Producer Int m b
forward = do
      x <- lift start
      yield x
      Pipes.unfoldr (fmap (Right . (\k -> (k, k))) . trans) x

observe :: MonadCond f => (Int, Maybe Double) -> f ()
observe (l, o) = when (isJust o) (factor $ normalPdf (emissionMean l) 1 (fromJust o))

backward :: (MonadInfer m) => [Double] -> Producer (Int, Maybe Double) m ()
backward dataset = zipWithM observe forward (each (Nothing : (Just <$> dataset)))

-- zipWithM :: (MonadInfer m) => [Double] -> Producer (Int, Maybe Double) m ()
zipWithM :: Monad m => ((a1, b) -> m ()) -> Producer a1 m r -> Producer b m r -> Proxy a' a2 () (a1, b) m r
zipWithM f p1 p2 = Pipes.zip p1 p2 >-> Pipes.chain f


syntheticData :: MonadSample m => Int -> m [Double]
syntheticData n = replicateM n syntheticPoint
  where
    syntheticPoint = uniformD [0, 1, 2]


run = enumerate $ hmm obs

obs = [2,2,2]
runPipes = enumerate $ reverse . take (length obs) <$> toListM (backward obs >-> Pipes.map fst)

-- ooh :: Enumerator ()
ooh :: MonadInfer m => Producer [(Bool, Double)] m r
ooh = Pipes.unfoldr (\x -> return (Right (enumerate x, do x' <- x; condition x'; return x'))) (bernoulli 0.5 :: Enumerator Bool)

r = enumerate $ toListM $ ooh >-> Pipes.take 3

  -- runEffect $ backward [] >-> undefined 

-- run2 :: SamplerIO String
run2 = sampleSTfixed $ prior (mh 100 $ hmm values)

run3 = sampleSTfixed $ runPopulation (rmsmcBasic 10 20 1 $ hmm values)
