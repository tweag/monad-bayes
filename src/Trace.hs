{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances,
  GADTs,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  Rank2Types,
  ScopedTypeVariables,
  StandaloneDeriving,
  UndecidableInstances
 #-}


module Trace where

import Control.Arrow
import Control.Monad (liftM, liftM2, mplus)
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.List (unfoldr, intercalate)
import Data.Typeable
import Data.Number.LogFloat hiding (sum)
import System.Random (mkStdGen)
import Text.Printf

import Debug.Trace

import Primitive
import Base
import Dist (normalize)
import Inference
import Sampler



data Cache where
  Cache :: Primitive a -> a -> Cache

refresh :: (MonadDist m) => Cache -> m Cache
refresh (Cache d x) = do
  x' <- primitive d
  return $ Cache d x'

-- Print Cache for debugging
instance Show Cache where
  show (Cache (Normal m s) x) = printf "[%.3f<-N%.3f|%.3f]" x m s
  show (Cache (Gamma  a b) x) = printf "[%.3f<-G%.3f|%.3f]" x a b
  show (Cache (Beta   a b) x) = printf "[%.3f<-B%.3f|%.3f]" x a b
  -- print Booleans for debugging
  show (Cache (Categorical ps) x) =
    case cast (x, ps) :: Maybe (Bool, [(Bool, LogFloat)]) of
      Nothing      -> "[cat(" ++ (tail $ init $ show $ map snd ps) ++ ")]"
      Just (x, ps) -> "[" ++ show x ++ "<-(" ++
                         (intercalate "," $ map (\(b,p) -> head (show b) : printf "%.3f" (fromLogFloat p)) ps)
                         ++ ")]"

class Monoid r => RandomDB r where
  {-# MINIMAL (singleton | record), consume, mutate, size, weight, resampled #-}
  -- constructors
  record    :: Primitive a -> a -> r
  record d x = singleton (Cache d x)
  singleton :: Cache -> r
  singleton (Cache d x) = record d x

  -- mutators
  consume   :: r -> Maybe (Cache, r)
  mutate    :: (MonadDist m) => r -> m r

  size :: r -> Int
  weight :: r -> LogFloat
  resampled :: r -> r -> r

  -- proposalFactor old new = q(old, new) / (weight $ resampled old new)
  --
  -- By default, assume that the proposal resamples a stochastic choice
  -- in the old trace uniformly at random. In this case, the proposal factor is
  -- the reciprocal of the number of stochastic choices in the old trace.
  proposalFactor :: r -> r -> Maybe LogFloat
  proposalFactor old new = let denom = size old in if denom /= 0 then Just (1 / fromIntegral denom) else Nothing

-- Correct usage:
-- `old` and `new` traces should both be feasible and
-- the probability of proposing from each other should be
-- positive.
--
-- If `fromJust` throws "Non-exhaustive pattern" error,
-- then the caller did not conform to correct usage.
mhCorrectionFactor :: (RandomDB r) => r -> r -> LogFloat
mhCorrectionFactor old new = fromJust $ do
    pf_new_old <- proposalFactor new old
    pf_old_new <- proposalFactor old new
    let numerator   = weight new * weight (resampled new old) * pf_new_old
    let denominator = weight old * weight (resampled old new) * pf_old_new
    if denominator == 0 then
      Nothing
    else
      Just $ numerator / denominator
      -- should not take mininimum with 1 here

-- | An old primitive sample is reusable if both distributions have the
-- same type and if old sample does not decrease acceptance ratio by
-- more than a threshold.
reusablePrimitive :: Primitive a -> a -> Primitive b -> Maybe ((b, LogFloat), b -> Bool)
reusablePrimitive d x d' =
  let
    threshold = 0.0
  in
    reusablePrimitiveDouble threshold d x d' `mplus` reusableCategorical threshold d x d'

-- | Try to reuse a sample from a categorical distribution
-- if it does not decrease acceptance ration by more than a threshold.
reusableCategorical :: LogFloat -> Primitive a -> a -> Primitive b -> Maybe ((b, LogFloat), b -> Bool)
reusableCategorical threshold d@(Categorical _) x d'@(Categorical _) = do
  x' <- cast x
  let pOld = pdf d x
  let pNew = pdf d' x'
  if pOld > 0 && pNew / pOld > threshold then
    Just ((x', pNew), (== x'))
  else
    Nothing
reusableCategorical threshold _ _ _ = Nothing

-- | An old primitive sample of type Double is reused if old sample does
-- not decrease acceptance ratio by more than a threshold.
-- In particular, a sample is always reused if its distribution did not
-- change, since it does not decrease acceptance ratio at all.
reusablePrimitiveDouble :: LogFloat -> Primitive a -> a -> Primitive b -> Maybe ((b, LogFloat), b -> Bool)
reusablePrimitiveDouble threshold d x d' =
  case (isPrimitiveDouble d, isPrimitiveDouble d') of
    (Just (from, to), Just (from', to')) ->
      let
        x' = from' $ to x
        pOld = pdf d x
        pNew = pdf d' x'
      in
        if pOld > 0 && pNew / pOld > threshold then
          Just ((x', pNew), (== to' x') . to')
        else
          Nothing
    otherwise ->
      Nothing

-- | Test whether a primitive distribution has type @Double@,
-- output conversions to and from if it does.
isPrimitiveDouble :: Primitive a -> Maybe (Double -> a, a -> Double)
isPrimitiveDouble (Normal _ _) = Just (id, id)
isPrimitiveDouble (Gamma  _ _) = Just (id, id)
isPrimitiveDouble (Beta   _ _) = Just (id, id)
isPrimitiveDouble _            = Nothing

-- | Return whether a sample @x@ drawn from @d@ is reused
-- as a sample $x'$ drawn from $d'$.
reusedSample :: Primitive a -> a -> Primitive b -> b -> Bool
reusedSample d x d' x' =
  case reusablePrimitive d x d' of
    Just (_, isReused) -> isReused x'
    Nothing      -> False

-----------------------------
-- CACHE LISTS AS RANDOMDB --
-----------------------------

instance RandomDB [Cache] where
  singleton = (: [])
  consume []       = Nothing
  consume (x : xs) = Just (x, xs)
  mutate [] = return []
  mutate xs = do
    i <- uniformD [0 .. length xs - 1]
    let (prefix, cache : postfix) = splitAt i xs
    cache' <- refresh cache
    return $ prefix ++ cache' : postfix
  size = length
  weight = foldr (\(Cache d x) p -> pdf d x * p) 1
  resampled old [] = []
  resampled [] new = new
  resampled (Cache d x : old) (Cache d' x' : new) | reusedSample d x d' x' = resampled old new
  resampled (cache : old) (cache' : new) = cache' : resampled old new

------------------------------
-- TRACET MONAD TRANSFORMER --
------------------------------

newtype TraceT r m a = TraceT (WriterT r m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

deriving instance (Monoid r, Monad m) => MonadWriter r (TraceT r m)

runTraceT :: TraceT r m a -> m (a, r)
runTraceT (TraceT writer) = runWriterT writer

-- WriterT preserves MonadDist
instance (RandomDB r, MonadDist m) => MonadDist (TraceT r m) where
  primitive d = do
    x <- lift $ primitive d
    tell $ record d x
    return x

-- WriterT preserves MonadBayes
instance (RandomDB r, MonadBayes m) => MonadBayes (TraceT r m) where
  factor = lift . factor
  condition = lift . condition

------------------------------
-- REUSET MONAD TRANDFORMER --
------------------------------

newtype ReuseT r m a = ReuseT (StateT r m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

deriving instance (Monad m) => MonadState r (ReuseT r m)

runReuseT :: ReuseT r m a -> r -> m (a, r)
runReuseT (ReuseT state) = runStateT state

-- Reuse samples of primitive distributions in the RandomDB
instance (RandomDB r, MonadDist m) => MonadDist (ReuseT r m) where
  primitive d' = do
    r <- get
    let resample = lift $ primitive d'
    case consume r of
      Nothing ->
        resample
      Just (Cache d x, r') -> do
        put r'
        maybe resample return (fmap (fst . fst) $ reusablePrimitive d x d')

type UpdaterT r m a = TraceT r (ReuseT r m) a
runUpdaterT = runReuseT . runTraceT

mhKernel'  :: (RandomDB r, MonadDist m) => r -> UpdaterT r m a -> MHKernel m (a, r)
mhKernel' = const mhKernel

mhKernel :: (RandomDB r, MonadDist m) => UpdaterT r m a -> MHKernel m (a, r)
mhKernel program = MHKernel $ \(x, r) -> do
  ((x', r'), leftover) <- runUpdaterT program r
  return ((x', r'), mhCorrectionFactor r r')

-----------------------
-- DEBUG EXPERIMENTS --
-----------------------

data Stat = Stat
  { accepted :: Bool
  , ratio :: LogFloat
  , oldSize :: Int
  , newSize :: Int
  , resampledSize :: Int
  }
{-
mhRun :: (forall m. (MonadBayes m) => m a) -> Int -> Int -> [a]
mhRun p seed steps = fst $ mhRunWithDebugger (idDebugger None) p seed steps

idDebugger :: r -> r -> [a] -> [a]
idDebugger _ _ = id

-- May be useful independently
newtype PriorT m a = PriorT { runPriorT :: StateT LogFloat m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

deriving instance (Monad m) => MonadState LogFloat (PriorT m)

instance (MonadDist m) => MonadDist (PriorT m) where
  primitive = lift . primitive

instance (MonadDist m) => MonadBayes (PriorT m) where
  condition True  = return ()
  condition False = put 0
  factor k = get >>= put . (k *)


mhRunWithDebugger :: forall a r. (RandomDB r, Monoid r) =>
                     (r -> [a] -> [a]) ->
                     (forall m. (MonadBayes m) => m a) ->
                     Int -> Int -> ([a], [Stat])
mhRunWithDebugger debugger program seed steps =
  if steps <= 0 then
    ([], [])
  else
    let
      program' = program :: PriorT (TraceT r Sampler) a
      tracer = runStateT (runPriorT program') 1 :: TraceT r Sampler (a, LogFloat)
      sampler = getTrace' tracer :: Sampler ((a, LogFloat), r)
    in
      flip sample (mkStdGen seed) $ do
        ((x, _), _) <- sampler -- run program once just to get a result
        (samples, stats) <- loop steps ((x, 0), nil)
        return (samples, stats)
  where
    loop :: Int -> ((a, LogFloat), r) -> Sampler ([a], [Stat])
    -- done
    loop steps _ | steps <= 0 = return ([], [])
    -- iterate
    loop steps ((x, k), r) = do
      r0 <- mutate r

      -- compute new RandomDB
      let program' = program :: PriorT (ReuseT r Sampler) a
      let reuser = runStateT (runPriorT program') 1 :: ReuseT r Sampler (a, LogFloat)
      let sampler = reuseTrace reuser r0 :: Sampler ((a, LogFloat), r)
      ((x1, k1), r1) <- sampler

      let ratio = if k == 0 then
                    1.0
                  else
                    fromMaybe 1 $ do
                      correction <- mhCorrectionFactor r r1
                      return $ min 1 (k1 / k * correction)

      accept <- bernoulli ratio

      let result@((x', k'), r') = if accept then ((x1, k1), r1) else ((x, k), r)

      (otherSamples, otherStats) <- loop (steps - 1) result

      let samples = x' : debugger r' otherSamples
      let stat = Stat accept ratio (size r) (size r1) (size $ resampled r r1)

      return (samples, stat : otherStats)

mhDebug :: (forall m. (MonadBayes m) => m a) -> Int -> Int -> [a]
mhDebug p seed size = fst $ mhRunWithDebugger ((trace . ("  " ++) . show) :: BindTree -> [a] -> [a]) p seed size

-- Run a sampler many times to produce many samples.
-- A reference to check MH against.
sampleMany :: Sampler a -> Int -> Int -> [a]
sampleMany sampler seed size = sample (sequence $ replicate size $ sampler) (mkStdGen seed)

mhDebugHistogram :: Histo -> (forall m. (MonadBayes m) => m Double) -> Int -> Int -> IO ()
mhDebugHistogram histo p seed steps = do
  let (samples, stats) = mhRunWithDebugger (idDebugger None) p seed steps

  let accStats = filter accepted stats

  histogram histo samples

  -- histogram configuration for plotting percentages
  let rateHisto = Histo 0 0.05 1.0 0 (cols histo)

  let totalAccepted = length (filter accepted stats)
  let totalSampled = length stats
  let acceptanceRate = fromIntegral totalAccepted / fromIntegral totalSampled :: Double
  putStrLn $ printf "Acceptance rate = %04.2f %%" (100 * acceptanceRate)
  histogram rateHisto (map (fromLogFloat . ratio) stats)

  -- Compute average reuse of old trace in all proposals,
  -- including rejected ones.
  let totalNewSize = sum $ map newSize stats
  let totalResampled = sum $ map resampledSize stats
  let totalReused = totalNewSize - totalResampled
  let reuseRate = fromIntegral totalReused / fromIntegral totalNewSize :: Double
  putStrLn $ printf "Reuse rate = %04.2f %%" (100 * reuseRate)
  histogram rateHisto (map (\s -> 1 - fromIntegral (resampledSize s) / fromIntegral (newSize s)) stats)
-}
data Histo = Histo { xmin :: Double -- lower bound of samples
                   , step :: Double -- size of bins
                   , xmax :: Double -- upper bound of samples
                   , ymax :: Double -- maximum density value to plot as a bar
                   , cols :: Int    -- number of characters in longest bar
                   }

-- | plot @[Double]@ as histogram
histogram :: Histo -> [Double] -> IO ()
histogram (Histo xmin step xmax ymax cols) xs0 =
  let
    -- remove out of bound data from the pool
    xs = filter (\x -> xmin <= x && x <= xmax) xs0
    -- create the next interval starting from x
    --
    --  nextInterval :: Double -> Maybe ((Double -> Bool, Double), Double)
    --                  ^^^^^^            ^^^^^^^^^^^^^^  ^^^^^^   ^^^^^^
    --                  lower bound       interval        middle   upper bound
    nextInterval x = if x >= xmax then
                       Nothing
                     else
                       let y = x + step in Just ((\z -> x <= z && z < y, x + 0.5 * step), y)
    intervals = unfoldr nextInterval xmin
    size = length xs
    bins = map (flip filter xs . fst) intervals
    mids = map snd intervals
    range = step * fromIntegral (length bins)
    -- 1 == sum_{ys in bins} factor * step * (length ys)
    --   == factor * step * size
    factor = 1 / (step * fromIntegral size)
    densities = map (\ys -> factor * fromIntegral (length ys)) bins
    maxDensity = maximum densities
    ymax' = if ymax == 0 && maxDensity == 0 then
              1
            else if ymax == 0 then
              maxDensity
            else
              ymax
    bars = map (\d -> concat $ replicate (min cols $ round (d / ymax' * fromIntegral cols)) "#") densities
    annotatedBars = zipWith3 (printf "%6.3f @ %6.3f %s") densities mids bars
  in
    putStrLn $ unlines annotatedBars

-- Successive lines in do-notation generates right-skewed trace.
-- Example:
--
--   mhDebug gaussians 0 10
--
gaussians :: MonadDist m => m Double
gaussians = do
  x <- normal 2 0.5
  y <- normal 3 0.5
  return $ x + y

-- Nested do-notation generates left-subtrees.
-- Example:
--
--   mhDebug deps 0 10
--
deps :: MonadDist m => m Double
deps = do
  x <- do
    z <- normal 5 0.6
    gamma z 0.5
  y <- beta x 0.4
  return y

-- Program with a variable number of random choices.
-- Example:
--
--   mhDebug varChoices 1 10
--   mhDebugHistogram (Histo (-2.5) 1 27.5 0.35 60) varChoices 0 20000
--   histogram (Histo (-2.5) 1 27.5 0.35 60) $ sampleMany varChoices 0 20000
--
-- Rate of reuse is low because the program makes 1 to 3 random choices
-- most of the time, so x is resampled with significant probability.
varChoices :: MonadDist m => m Double
varChoices = do
  -- Use Gaussian to mimic geometric, since geometric
  -- is implemented in terms of categorical, and we don't
  -- support categorical yet.
  x <- normal 0 5
  let n = floor (abs x / 2)

  xs <- sequence $ replicate n $ normal (abs x) 1
  return $ sum xs

-- Figure 8b of
-- Hur, Nori, Rajamani, Samuel:
-- A provably correct sampler for probabilistic programs.
--
-- Not using fig 2 because I don't know how they parametrize the
-- Gamma distribution. Histogram produced by MH looks okay so far.
-- In fact, it agrees with WolframAlpha better than the histogram
-- in Hur et al. The maximum density of Normal 10 2 is 0.19947,
-- so the maximum density of fig8b on positive reals should be
-- half of that, i. e., around 0.1.
--
-- Examples:
--
--   mhDebugHistogram (Histo (-4.25) 0.25 19.25 0.5 60) fig8b 0 10000
--   histogram (Histo (-4.25) 0.25 19.25 0.5 60) $ sampleMany fig8b 0 10000
--
fig8b :: MonadDist m => m Double
fig8b = do
  x <- normal 0 1
  if x > 0 then
    normal 10 2
  else
    return x

-- Grass model: returning 0 or 1 for histogram.
-- Can only get 1 significant digit in 20k samples;
-- try seeds 0, 300, 1997, 314159.
--
-- Should compare with sampling from posterior.
--
-- Examples:
--
--   mhDebug grassModel 0 10
--   mhDebugHistogram (Histo (-0.5) 1.0 1.5 1.0 60) grassModel 0 2000
--   enumerate grassModel -- after import Dist
--
grassModel :: MonadBayes m => m Double
grassModel = do
  let flip p  = categorical [(True, p), (False, 1 - p)]
  let m <&& b = liftM2 (&&) m (return b)
  let (<||>)  = liftM2 (||)
  rain       <- flip 0.3
  sprinkler  <- flip 0.5
  grassIsWet <- (flip 0.9 <&& rain)
           <||> (flip 0.8 <&& sprinkler)
           <||> flip 0.1
  condition grassIsWet
  return $ if rain then 1.0 else 0.0
