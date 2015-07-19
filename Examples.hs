{-# LANGUAGE TupleSections #-}

module Examples where

import Base
import Dist
import Exact
import Explicit
import Sampler
import SMC
import MCMC
import GP

import Testing.HMMexact (hmmExact, hmmExactMarginal)
import qualified Testing.DPexact as DP

import Data.Random.Distribution.Beta (Beta(Beta))
import Data.Random.Distribution.Uniform (Uniform(Uniform))
import Data.Random.Distribution.Normal (Normal(Normal))
import Data.Random.Distribution.Bernoulli (Bernoulli(Bernoulli))
import Data.Random.Distribution.Gamma (Gamma(Gamma))
import Data.Random.Distribution (pdf)

import Numeric.LinearAlgebra.HMatrix
import Control.Monad (liftM, liftM2, mapM)
import Data.List (partition, nub)
import Data.Maybe (mapMaybe)

-----------------------------------------------------
--HMM

hmm :: (Functor d, Monad d) => d a -> (a -> d a) -> (a -> d b) -> d ([a],[b])
-- | A Hidden Markov Model defines a distribution over a pair of two lists:
-- a list of latent states and a list of observables.
-- Supports both finite and infinite cases.
hmm start trans gen =
 let
  construct x = fmap (x:) (trans x >>= construct)
 in
  do
    states      <- start >>= construct
    observables <- mapM gen states
    return (states, observables)

finiteHMM :: (Functor d, Monad d) => Int -> d a -> (a -> d a) -> (a -> d b) -> d ([a],[b])
-- | A Hidden Markov Model defines a distribution over a pair of two lists:
-- a list of latent states and a list of observables.
-- Finite list version.
finiteHMM n start trans gen =
 let
  construct 0 _ = return []
  construct n x = fmap (x:) (trans x >>= construct (n-1))
 in
  do
    states      <- start >>= construct n
    observables <- mapM gen states
    return (states, observables)

------------------------------------------------------------
--Dirichlet Processes

stick :: (Monad d, DiscreteDist d) => [Prob] -> [a] -> d a
-- | Stick-breaking function.
stick (b:breaks) (a:atoms) =
    choice b (certainly a) (stick breaks atoms)

dp :: (Functor d, Monad d, Sampler d, DiscreteDist d, ContinuousDist d) =>
        Double -> d a -> d (d a)
-- | A Dirichlet Process generates a random probability distribution
-- using the stick-breaking representation.
dp concentration base = do
  breaks <- sequence $ repeat $ fmap prob $ beta 1 concentration
  atoms  <- sequence $ repeat base
  return $ stick breaks atoms

dpData = [1.0,1.1,1.2,-1.0,-1.5,-2.0,0.001,0.01,0.005,0.0]

--dpData = [0.0,10.0]

dpMixture :: Dist [Int]
dpMixture =
  let
    --lazily generate clusters
    clusters = do
      let atoms = [1..]
      breaks <- sequence $ repeat $ fmap prob $ beta 1 1
      let classgen = stick breaks atoms
      vars <- sequence $ repeat $ fmap (1/) $ gamma 1 1
      means <- mapM (normal 0) vars
      return (classgen, vars, means)
    obs = [1.0,1.1,1.2,-1.0,-1.5,-2.0,
           0.001,0.01,0.005,0.0]
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
      prob $ pdf (Normal mean (sqrt var)) y

  in
   --exctract cluster assignments
   fmap (reverse . map (\(x,_,_) -> x) . snd) points

dpClusters = fmap (maximum . normalizePartition) dpMixture

normalizePartition :: [Int] -> [Int]
normalizePartition xs = mapMaybe (`lookup` trans) xs where
  trans = zip unique [1..]
  unique = nub xs

dpFinal :: Dist [Int]
dpFinal = fmap (normalizePartition) dpMixture

dpPrior = (0, 1, 1, 1)

dpMixtureExact :: [Int] -> Prob
dpMixtureExact = prob . DP.exact dpPrior dpData

dpClusterExact :: Int -> Prob
dpClusterExact = prob . DP.exactCluster dpPrior dpData

------------------------------------------------------------
--Coin tossing

coinToss :: (Functor d, Monad d, Conditional d, Sampler d) => Double -> Double -> [Bool] -> d Prob
-- | An infinite sequence of coin tosses.
-- The prior on the coin weight is 'Beta' alpha beta.
coinToss alpha beta results =
    register prior results where
        prior = fmap prob $ external $ Beta alpha beta
        register d [] = d
        register d (r:rs) = register (observe d (bernoulli :: Prob -> Explicit Bool) r) rs

coinExact :: Prob -> Prob -> [Bool] -> Beta Prob
-- | Exact posterior for the 'coinToss' example.
coinExact alpha beta results =
    Beta (alpha + positive) (beta + negative) where
        (p,n) = partition id results
        positive = fromIntegral $ length p
        negative = fromIntegral $ length n

------------------------------------------------------------
--Die rolling

die :: (Monad d, DiscreteDist d, Integral n) => n -> d n
-- | A distribution over the sums of results of n independent rolls of a fair die
die 0 = return 0
die 1 = uniform [1,2,3,4,5,6]
die n = liftM2 (+) (die 1) (die (n-1))

conditionalDie n = condition (\n -> 1 / fromIntegral n) (die n)

------------------------------------------------------------
--Anglican examples

anglicanHMMValues :: [Double]

anglicanHMMStates = [0,1,2]
anglicanHMMStart  = replicate n (1 / fromIntegral n) where n = length anglicanHMMStates
anglicanHMMTrans 0 = [0.1 ,0.5 ,0.4]
anglicanHMMTrans 1 = [0.2 ,0.2 ,0.6]
anglicanHMMTrans 2 = [0.15,0.15,0.7]
anglicanHMMValues = [0.9,0.8,0.7,0,-0.025,5,2,0.1,0,0.13,0.45,6,0.2,0.3,-1,-1]
anglicanHMMMean 0 = -1
anglicanHMMMean 1 = 1
anglicanHMMMean 2 = 0
anglicanHMMWeight s = pdf (Normal (anglicanHMMMean s) 1)

anglicanHMMInit = fromList anglicanHMMStart
anglicanHMMMatrix = fromColumns (map (fromList . anglicanHMMTrans) anglicanHMMStates)
anglicanHMMScores = map (\y -> fromList (map (`anglicanHMMWeight` y) anglicanHMMStates)) anglicanHMMValues

anglicanHMMExact = prob . hmmExact (fromList anglicanHMMStart) anglicanHMMMatrix anglicanHMMScores

anglicanHMMExactMarginals = hmmExactMarginal (fromList anglicanHMMStart) anglicanHMMMatrix anglicanHMMScores

anglicanHMM :: (Functor d, Monad d, DiscreteDist d, Conditional d) => d [Int]
anglicanHMM = fmap (tail. take (length values + 1) . fst) $ score (length values) (hmm init trans gen) where
    states = anglicanHMMStates
    init = categorical $ zip states $ map prob anglicanHMMStart
    trans = categorical . zip states . map prob . anglicanHMMTrans
    gen = return . anglicanHMMMean
    values = anglicanHMMValues
    addNoise = flip Normal 1
    score 0 d = d
    score n d = score (n-1) $ condition (prob . (`pdf` (values !! (n - 1)))
                                              . addNoise . (!! n) . snd) d

anglicanFiniteHMM :: (Functor d, Monad d, Conditional d, DiscreteDist d, Integral n) => d [n]
anglicanFiniteHMM = fmap (tail . fst) $ score (length values) (finiteHMM (length values + 1) init trans gen) where
    states = [0,1,2]
    init = uniform states
    trans 0 = categorical $ zip states [0.1,0.5,0.4]
    trans 1 = categorical $ zip states [0.2,0.2,0.6]
    trans 2 = categorical $ zip states [0.15,0.15,0.7]
    gen 0 = certainly (-1)
    gen 1 = certainly 1
    gen 2 = certainly 0
    values = anglicanHMMValues
    addNoise = flip Normal 1
    score 0 d = d
    score n d = score (n-1) $ condition (prob . (`pdf` (values !! (n - 1)))
                                              . addNoise . (!! n) . snd) d

anglicanStepHMM :: (Functor d, Monad d, Conditional d, DiscreteDist d) => d [Int]
anglicanStepHMM = fmap (tail . reverse) $ generate anglicanHMMValues $ fmap (:[]) init where
    trans = categorical . zip states . anglicanHMMTrans
    init = categorical $ zip states $ map prob anglicanHMMStart
    states = anglicanHMMStates
    generate [] d = d
    generate (y:ys) d = generate ys d' where
        d' = condition (prob . (`anglicanHMMWeight` y) . head) $ do
            rest <- d
            x    <- trans $ head rest
            return (x:rest)



---------------------------------------------------
--Linear regression
--Three equivalent definitions of a linear regression model

linearConcrete :: Dist (Double -> Double)
linearConcrete =
  Bind (normal 0 1) (\a ->
    Bind (normal 0 1) (\b -> Return (\x -> a*x + b)) )

linearMonad :: Dist (Double -> Double)
linearMonad =
  normal 0 1 >>= \a ->
  normal 0 1 >>= \b ->
  return (\x -> a*x + b)

linearDo :: Dist (Double -> Double)
linearDo = do
  a <- normal 0 1
  b <- normal 0 1
  return (\x -> a*x + b)
