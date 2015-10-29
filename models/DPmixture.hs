{-# LANGUAGE
 TupleSections,
 FlexibleContexts
 #-}

module DPmixture (
                  stick,
                  dp,
                  dpMixture,
                  dpClusters,
                  posteriorClustersDist
                 ) where

-- Dirichlet Process mixture of Gaussians

import qualified Data.Random.Distribution.Normal as Ext
import Data.Random (pdf)
import Data.List
import Data.Maybe
import Data.Ix (range)
import Numeric.SpecFunctions (logGamma, factorial)

import Base
import Dist

type NormalInvGamma = (Double,Double,Double,Double)

-- | Prior over cluster parameters used in 'dpMixture'
params :: NormalInvGamma
params = (0,1/10,1,10)
(m,k,a,b) = params

obs :: [Double]
obs = [1.0,1.1,1.2,-1.0,-1.5,-2.0,0.001,0.01,0.005,0.0]

-- | Stick-breaking function.
stick :: (Monad d, Bernoulli d) => [Prob] -> [a] -> d a
stick (b:breaks) (a:atoms) =
    choice b (return a) (stick breaks atoms)

-- | A Dirichlet Process generates a random probability distribution
-- using the stick-breaking representation.
dp :: (Functor d, Monad d, Sampler d, Bernoulli d, Beta d) =>
        Double -> d a -> d (d a)
dp concentration base = do
  breaks <- sequence $ repeat $ fmap prob $ beta 1 concentration
  atoms  <- sequence $ repeat base
  return $ stick breaks atoms

-- | DP mixture example from http://dl.acm.org/citation.cfm?id=2804317
dpMixture :: (Functor d, Monad d, Bernoulli d, Normal d, Beta d, Gamma d, Conditional d) => d [Int]
dpMixture =
  let
    --lazily generate clusters
    clusters = do
      let atoms = [1..]
      breaks <- sequence $ repeat $ fmap prob $ beta 1 1
      let classgen = stick breaks atoms
      vars <- sequence $ repeat $ fmap ((1/) . (*k)) $ gamma a b
      means <- mapM (normal m) vars
      return (classgen, vars, means)
    n = length obs
    --start with no data points
    start = fmap (,[]) clusters

    --add points one by one
    points = foldl build start obs
    build d y = condition (score y . head . snd) $ do
      (clusters, rest) <- d
      let (classgen, vars, means) = clusters
      cluster <- classgen
      let point = (cluster, vars !! cluster,
                   means !! cluster)
      return (clusters, point : rest)

    --the likelihood in a cluster is Gaussian
    score y (cluster, var, mean) =
      -- Normal mean stddev
      prob $ pdf (Ext.Normal mean (sqrt var)) y

  in
   --exctract cluster assignments
   fmap (reverse . map (\(x,_,_) -> x) . snd) points

-- | 'dpMixture' restricted to the number of clusters only
dpClusters :: (Functor d, Monad d, Bernoulli d, Normal d, Beta d, Gamma d, Conditional d) => d Int
dpClusters = fmap (maximum . normalizePartition) dpMixture

-- | Renames identifiers so that they range from 1 to n
normalizePartition :: [Int] -> [Int]
normalizePartition xs = mapMaybe (`lookup` trans) xs where
  trans = zip unique [1..]
  unique = nub xs



----------------------------------------------------------
-- Exact posterior

-- | Posterior for parameters of a cluster
posterior :: NormalInvGamma -> [Double] -> NormalInvGamma
posterior (m,k,a,b) xs = (m',k',a',b') where
    m' = (k * m + n * x) / k'
    k' = k + n
    a' = a + n / 2
    b' = b + (m * m * k + x2 - m' * m' * k') / 2 --s / 2 + (k * n * (x - m) ^ 2) / (2 * (k + n))
    n  = fromIntegral $ length xs
    x  = sum xs / n  -- mean xs
    x2 = sum $ zipWith (*) xs xs  -- sum xs^2
    s  = sum $ zipWith (*) d d where  -- pvar xs
            d = map (+ (- x)) xs

-- | Model evidence for a cluster
evidence :: NormalInvGamma -> [Double] -> Double
evidence prior xs = exp (logGamma a' - logGamma a) * (b ** a / b' ** a') * sqrt (k / k') * c ^ n where
    (m,k,a,b)     = prior
    (m',k',a',b') = posterior prior xs
    c             = 1 / (2 * (sqrt pi))
    n             = length xs


-- | CRP prior over partitions
crp :: [Int] -> Double
crp labels = (product $ map factor counts) / factorial n where
    factor x = factorial (x - 1)
    n = length labels
    counts = map (\x -> length (filter (== x) labels)) clusters
    clusters = nub labels

-- | DP mixture likelihood of a partition
mixture :: NormalInvGamma -> [Double] -> [Int] -> Double
mixture prior xs ks = (product $ map (evidence prior) clusters) * crp ks where
  n = length xs
  clusters = map extract labels
  labels = nub ks
  extract n = map fst $ filter ((== n) . snd) points
  points = zip xs ks

-- | Exact posterior for a partition
exact :: NormalInvGamma -> [Double] -> [Int] -> Double
exact prior xs = let
  z = sum $ map (mixture prior xs) (partitions n)
  n = length xs
  f partition = mixture prior xs partition / z
  in
   f

-- | Exact posterior over the number of clusters
exactClusters :: NormalInvGamma -> [Double] -> Int -> Double
exactClusters prior xs = let
  partitionScore = exact prior xs
  results = map score [1..n]
  score k = sum $ map partitionScore $ filter ((== k) . maximum) $ partitions n
  n = length xs
  lookup i = results !! (i - 1)
  in
   lookup

-- | All possible partitions
partitions :: Int -> [[Int]]
partitions n = generate n 1 where
  generate 0 _ = [[]]
  generate n k = do
    x <- range (1,k)          -- range is inclusive on both ends
    let k' = max k (x+1)      -- next empty cluster
    xs <- generate (n-1) k'
    return (x:xs)

-- | Posterior over the number of clusters
posteriorClustersDist :: Categorical Int d => d Int
posteriorClustersDist = categorical $ zip ns $ map lookup ns where
    ns = [1 .. length obs]
    lookup = prob . exactClusters params obs
