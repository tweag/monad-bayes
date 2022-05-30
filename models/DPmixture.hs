{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module DPmixture where

-- Dirichlet Process mixture of Gaussians

-- Dirichlet Process mixture of Gaussians
import Control.Applicative ((<$>), Applicative (liftA2))
-- import Control.Monad.Memo (memo, startEvalMemoT)
-- import Control.Monad.Trans.Memo.StateCache

import Data.Ix (range)
import Data.List 
import Data.Maybe ( mapMaybe, fromMaybe )
-- import Numeric.SpecFunctions (factorial, logGamma)
import Prelude hiding (sum)
import Control.Monad.Bayes.Enumerator ( compact )
import Control.Monad.Bayes.Class
    ( normalPdf,
      factor,
      MonadInfer,
      MonadSample(categorical, bernoulli, beta, gamma, normal) )
import Control.Monad.Bayes.Sampler ( sampleIO ) 
import Control.Monad.Bayes.Weighted ( prior ) 
import Control.Monad.Bayes.Traced ( mh ) 
import Control.Monad ( replicateM )
import Pipes.Prelude qualified as P
import Pipes (Pipe, await, yield, lift, (>->), Producer, runEffect)
import System.IO.Unsafe ()
import Data.Map qualified as M
import Data.Vector qualified as V
import Debug.Trace ( trace )

type NormalInvGamma = (Double, Double, Double, Double)

newtype Table = Table Int deriving newtype (Eq, Ord, Num, Show)
type SeatingPlan = M.Map Table Int 

logGamma = undefined
factorial = undefined

-- | Prior over cluster parameters used in 'dpMixture'
params :: NormalInvGamma
params = (0, 1 / 10, 1, 10)

m, k, a, b :: Double
(m, k, a, b) = params

obs :: [Double]
obs = [1.0, 1.1, 1.2, -1.0, -1.5, -2.0, 0.001, 0.01, 0.005, 0.0]

-- | Stick-breaking function.
stick :: MonadSample m => [Double] -> [a] -> m a
stick (b : breaks) (a : atoms) = do
  stop <- bernoulli b
  if stop then return a else stick breaks atoms

-- | A Dirichlet Process generates a random probability distribution
-- using the stick-breaking representation.
dp :: MonadSample m => Double -> m a -> m (m a)
dp concentration base = do
  breaks <- sequence $ repeat $ beta 1 concentration
  atoms <- sequence $ repeat base
  return $ stick breaks atoms




crpP :: MonadSample m => Double -> Producer SeatingPlan m r
crpP concentration = flip P.unfoldr M.empty \seatingPlan -> do
  let n = 1 + (fromIntegral $ sum $ M.elems seatingPlan)
      nAtTable t = fromMaybe 0 $ M.lookup (Table t) seatingPlan
      nOfTables = length $ M.keys seatingPlan 
  assignment <- categorical $ V.fromList
    $  fmap (/(n-1+concentration)) 
    $ (<> [concentration]) -- [0...m_k, concentration]
    $ fmap (fromIntegral . nAtTable) $
    take nOfTables [0..]
  let newSeatingPlan = M.insertWith (+) (Table assignment) 1 seatingPlan
  return $ Right $ dup newSeatingPlan


test = do
  x <- sampleIO $ fmap last $ P.toListM $ crpP 0.5 >-> P.take 100
  print x


dpP concentration base = do
  let breaks = P.repeatM $ liftA2 (,) (beta 1 concentration) base
  return $ fmap snd $ runEffect (breaks >-> takeWhile' (\(d,b) -> bernoulli d) >-> P.drain)
-- pr = unsafePerformIO $ sampleIOfixed $ dpP 0.5 (normal 0 1)


dpmP :: IO [(Bool, Double)]
dpmP = fmap (compact . (\x -> zip x (repeat 1.0))) $ sampleIO do
  mixingMeasure <- dpP 0.5 (bernoulli 0.5)
  replicateM 10 mixingMeasure

-- -- | DP mixture example from http://dl.acm.org/citation.cfm?id=2804317
dpMixture :: MonadInfer m => [Double] -> m [Int]
dpMixture obs = 
  let --lazily generate clusters
      clusters = do
        let atoms = [1..]
        breaks <- sequence $ repeat $ beta 1 1
        let classgen = stick breaks atoms
        vars <- sequence $ repeat $ recip . (* k) <$> gamma a b
        means <- mapM (normal m) vars
        return (classgen, vars, means)
      n = length obs
      --start with no data points
      start = fmap (,[]) clusters
      --add points one by one
      points = foldl build start obs
      build d y = do
        (clusters, rest) <- d
        let (classgen, vars, means) = clusters
        cluster <- classgen
        let mean = means !! cluster
        let var = vars !! cluster
        let point = (cluster, var, mean)
        factor $ normalPdf mean (sqrt var) y
        return (clusters, point : rest)
   in --extract cluster assignments
      fmap (reverse . map (\(x, _, _) -> x) . snd) points

-- -- | 'dpMixture' restricted to the number of clusters only
dpClusters :: MonadInfer m => [Double] -> m Int
dpClusters obs = fmap (maximum . normalizePartition) (dpMixture obs)

-- -- | Renames identifiers so that they range from 1 to n
normalizePartition :: [Int] -> [Int]
normalizePartition xs = mapMaybe (`lookup` trans) xs
  where
    trans = zip unique [1 ..]
    unique = nub xs

runDpMixture = 
  sampleIO $ prior $ mh 1 $ dpMixture obs

----------------------------------------------------------
-- Exact posterior

-- | Posterior for parameters of a cluster
posterior :: NormalInvGamma -> [Double] -> NormalInvGamma
posterior (m, k, a, b) xs = (m', k', a', b')
  where
    m' = (k * m + n * x) / k'
    k' = k + n
    a' = a + n / 2
    b' = b + (m * m * k + x2 - m' * m' * k') / 2 --s / 2 + (k * n * (x - m) ^ 2) / (2 * (k + n))
    n = fromIntegral $ length xs
    x = sum xs / n -- mean xs
    x2 = sum $ zipWith (*) xs xs -- sum xs^2
    s = sum $ zipWith (*) d d -- pvar xs
      where
        d = map (+ (- x)) xs

-- | Model evidence for a cluster
evidence :: NormalInvGamma -> [Double] -> Double
evidence prior xs = exp (logGamma a' - logGamma a) * (b ** a / b' ** a') * sqrt (k / k') * c ^ n
  where
    (m, k, a, b) = prior
    (m', k', a', b') = posterior prior xs
    c = 1 / (2 * sqrt pi)
    n = length xs

-- | CRP prior over partitions
crp :: [Int] -> Double
crp labels = product (map factor counts) / factorial n
  where
    factor x = factorial (x - 1)
    n = length labels
    counts = map (\x -> length (filter (== x) labels)) clusters
    clusters = nub labels

-- | DP mixture likelihood of a partition
mixture :: NormalInvGamma -> [Double] -> [Int] -> Double
mixture prior xs ks = product (map (evidence prior) clusters) * crp ks
  where
    n = length xs
    clusters = map extract labels
    labels = nub ks
    extract n = map fst $ filter ((== n) . snd) points
    points = zip xs ks

-- | Exact posterior for a partition
exact :: NormalInvGamma -> [Double] -> [Int] -> Double
exact prior xs =
  let z = sum $ map (mixture prior xs) (partitions n)
      n = length xs
      f partition = mixture prior xs partition / z
   in f

-- | Exact posterior over the number of clusters
exactClusters :: NormalInvGamma -> [Double] -> Int -> Double
exactClusters prior xs =
  let partitionScore = exact prior xs
      results = map score [1 .. n]
      score k = sum $ map partitionScore $ filter ((== k) . maximum) $ partitions n
      n = length xs
      lookup i = results !! (i - 1)
   in lookup

-- | All possible partitions
partitions :: Int -> [[Int]]
partitions n = generate n 1
  where
    generate 0 _ = [[]]
    generate n k = do
      x <- range (1, k) -- range is inclusive on both ends
      let k' = max k (x + 1) -- next empty cluster
      xs <- generate (n -1) k'
      return (x : xs)

-- | Posterior over the number of clusters
posteriorClustersDist :: (MonadSample m) => m Int
posteriorClustersDist = categorical $ V.map lookup ns
  where
    ns = V.fromList [1 .. length obs]
    lookup = exactClusters params obs

----------
-- helpers
----------

dup :: b -> (b, b)
dup x = (x,x)

takeWhile' :: Monad m => (a -> m Bool) -> Pipe a a m a
takeWhile' predicate = go
  where
    go = do
        a <- await
        b <- lift $ predicate a
        if b
            then do
                yield a
                go
            else return a


traceIt x = trace (show x) x 
