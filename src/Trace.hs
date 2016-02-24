{-# LANGUAGE
  GADTs,
  DeriveFunctor,
  ScopedTypeVariables
 #-}


module Trace where

import Control.Monad (liftM,liftM2)
import Data.List (unfoldr)
import Data.Number.LogFloat hiding (sum)
import System.Random
import Text.Printf

import Debug.Trace

import Primitive
import Base
import Sampler

-- | A random database of stochastic choices made in the program.
data RandomDB where
    None :: RandomDB
    Node :: Primitive a -> a -> RandomDB
    -- The arguments to Bind are: trace to this point, trace from this point.
    Bind :: RandomDB -> RandomDB -> RandomDB

-- print the stochastic choices for debugging
instance Show RandomDB where
  show None = "None"
  show (Node (Normal m s) x) = printf "(%.3f<-N%.3f|%.3f)" x m s
  show (Node (Gamma  a b) x) = printf "(%.3f<-G%.3f|%.3f)" x a b
  show (Node (Beta   a b) x) = printf "(%.3f<-B%.3f|%.3f)" x a b
  show (Bind t1 t2) = printf "(B %s %s)" (show t1) (show t2)

data TraceM a = TraceM { randomDB :: RandomDB, value :: a }
    deriving (Functor)

-- the monad instance below is used in `instance Applicative Trace` by `liftM2`
instance Applicative TraceM where
    pure = TraceM None
    (TraceM fTrace f) <*> (TraceM xTrace x) =
       TraceM (Bind fTrace xTrace) (f x)

instance Monad TraceM where
    (TraceM xTrace x) >>= f =
        let
          TraceM yTrace y = f x
        in
          TraceM (Bind xTrace yTrace) y

-- | The number of random choices in the RandomDB.
size :: RandomDB -> Int
size None = 0
size (Node _ _) = 1
size (Bind t1 t2) = size t1 + size t2

-- | The product of all densities in the trace.
weight :: RandomDB -> LogFloat
weight None = 1
weight (Node d x) = pdf d x
weight (Bind t1 t2) = weight t1 * weight t2

-- | Test whether a primitive distribution has type @Double@,
-- output conversions to and from if it does.
isPrimitiveDouble :: Primitive a -> Maybe (Double -> a, a -> Double)
isPrimitiveDouble (Normal _ _) = Just (id, id)
isPrimitiveDouble (Gamma  _ _) = Just (id, id)
isPrimitiveDouble (Beta   _ _) = Just (id, id)
isPrimitiveDouble _            = Nothing

-- | If both distributions have type Double and old sample has positive density
-- according to new distribution, then the old sample is reusable.
reusablePrimitive :: Primitive a -> a -> Primitive b -> Maybe b
reusablePrimitive d x d' =
  case (isPrimitiveDouble d, isPrimitiveDouble d') of
    (Just (from, to), Just (from', to')) | pdf d x > 0 && pdf d' (from' $ to x) > 0 ->
      Just $ from' $ to x
    otherwise ->
      Nothing

-- | Test whether two sample-distribution pairs are completely identical
sameSample :: Primitive a -> a -> Primitive b -> b -> Bool
sameSample (Normal m s) x (Normal m' s') x' = m == m' && s == s' && x == x'
sameSample (Gamma  a b) x (Gamma  a' b') x' = a == a' && b == b' && x == x'
sameSample (Beta   a b) x (Beta   a' b') x' = a == a' && b == b' && x == x'
sameSample _ _ _ _ = False

-- | Computes the parts of the new trace not in the old trace.
-- Used to compute proposal densities:
-- q(old, new) = weight (new `minus` old)
-- q(new, old) = weight (old `minus` new)
minus :: RandomDB -> RandomDB -> RandomDB
Node d' x' `minus` Node d x | sameSample d x d' x' = None
Bind t1' t2' `minus` Bind t1 t2 = Bind (t1' `minus` t1) (t2' `minus` t2)
new `minus` old = new

-- | From two @RandomDB@, compute the acceptance ratio.
-- Precondition: Program is @MonadDist@ but not @MonadBayes@
--               (i.e., it doesn't call @condition@ or @factor@),
--               so that the target density of an execution is completely
--               determined by the stochastic choices made.
--
-- Lemma. Write
--
--   C(x,y) = Pr[to update x, letmost node of y `minus` x is chosen].
--
-- Then in the absence of conditioning,
--
--   acceptance-ratio = C(y,x) / C(x,y).
--
--
-- Remark. The acceptance ratio depends on how the site of single site
-- update is chosen. In the previous version,
--
--   Pr[site X is updated] = 1 / 2^{depth of X},
--
-- and we have acceptance-ratio == 1 due to the symmetry
--
--   C(x,y) = Pr[to update x, letmost node of y `minus` x is chosen]
--          = 1 / 2^{depth of leftmost node of y `minus` x}
--          = 1 / 2^{depth of leftmost node of x `minus` y}
--          = C(y,x).
--
-- If we choose another single-site update strategy, e. g., pick
-- a uniformly random primitive sample in the trace to update,
-- then C(x,y) != C(y,x) and the acceptance ratio is not 1 any more.
--
--
-- Proof of lemma.
-- Without conditioning,
--
--   weight x = pi(x).
--
-- Observe that
--
--   q(x,y) = weight (y `minus` x) * C(x,y) = pi(y `minus` x) * C(x,y),
--
-- which means
--
--   pi(y `minus` x) = q(x,y) / C(x,y).
--
-- Recall further that
--
--   y = x `union` (y `minus` x) `minus` (x `minus` y).
--
-- Therefore
--
--   pi(new) = pi(old `union` (new `minus` old) `minus` (old `minus` new))
--           = pi(old) * pi(new `minus` old) / pi(old `minus` new)
--           = pi(old) * (q(old,new) / C(old,new)) / (q(new,old) * C(new,old))
--           = (pi(old) * q(old,new) / q(new, old)) * (C(new,old) / C(old,new)).
--
-- Now
--
--   pi(old) * q(old, new) = p(old) * pi(y `minus` x) * C(x,y) > 0
--
-- since all 3 factors are positive. The acceptance ratio is
--
--     (pi(new) * q(new, old)) / (pi(old) * q(old, new))
--
--   = (((pi(old) * q(old, new) / q(new, old)) * q(new, old)) / (pi(old) * q(old, new))) *
--       (C(new,old) / C(old,new))
--
--   = C(new,old) / C(old,new).
--
-- QED
--
-- The current update strategy chooses a primitive value uniformly at random.
-- If the old execution trace is not deterministic, then
--
--   C(new,old) = 1 / size new,
--   C(old,new) = 1 / size old,
--   acceptance-ratio = C(new,old) / C(old,new) = size old / size new.
--
-- If the old execution trace is deterministic, then the program makes
-- no random choice whatsoever. The trace space is a singleton, and
-- pi(old) = pi(new) = q(old,new) = q(new,old) = 1.
--
acceptanceRatio :: RandomDB -> RandomDB -> LogFloat
acceptanceRatio old new =
  let
    size_old = size old
    size_new = size new
  in
    if size_old * size_new == 0 then
      1
    else
      min 1 (fromIntegral size_old / fromIntegral size_new)

-- | Resample the i-th random choice
updateAt :: Int -> RandomDB -> Sampler RandomDB
updateAt n db =
  fmap (either id $ error $ printf "updateAt: index %d out of bound in $s" n (show db)) (loop n db)
  where
    -- Return either an updated @RandomDB@
    --            or the result of subtracting the number of traversed
    --               random choices from the index
    loop :: Int -> RandomDB -> Sampler (Either RandomDB Int)
    loop n db | n < 0 = error $ "loop: illegal index " ++ show n
    loop n None = return $ Right n
    loop n (Node d x) | n == 0 = fmap (Left . Node d) (primitive d)
    loop n (Node d x) | n > 0 = return $ Right (n - 1)
    loop n (Bind t1 t2) = do
      t1' <- loop n t1
      let keep_t2 = Left . flip Bind t2
      let keep_t1 = Left . Bind t1
      -- if t1 is updated, join the updated t1 together with t2.
      -- if t2 is updated, join t1 together with the updated t2.
      -- if neither is updated, return new index.
      either (return . keep_t2) (fmap (either keep_t1 Right) . flip loop t2) t1'

-- | Updates a trace by resampling a randomly selected site.
update :: RandomDB -> Sampler RandomDB
update r = do
  let n = size r
  if n == 0 then
    return r -- no random element in previous RandomDB
  else
    do
      let p = 1 / fromIntegral n
      i <- categorical $ map (flip (,) p) [0 .. (n - 1)]
      updateAt i r

-- | Split a @RandomDB@ into two if there is sufficient information
splitDB :: RandomDB -> (RandomDB, RandomDB)
splitDB (Bind t1 t2) = (t1, t2)
splitDB _ = (None, None)

-- | A traced program takes the stochastic choices of the previous execution
-- and generates a new value together with the stochastic choices leading up to it.
newtype Trace a = Trace (RandomDB -> Sampler (TraceM a))
runTrace (Trace d) = d

instance Functor Trace where
    -- modify final result, leaving stochastic choices intact
    fmap f (Trace p) = Trace $ fmap (fmap f) . p

instance Applicative Trace where
    pure = return
    (<*>) = liftM2 ($)

-- | Execute program, reusing previous trace
instance Monad Trace where
    return = Trace . const . return . return

    -- | @>>=@ handles reuse of @Bind@ nodes
    t >>= f = Trace $ \db ->
        do
          let (db1, db2) = splitDB db
          r1 <- runTrace t db1
          r2 <- runTrace (f $ value r1) db2
          return $ r1 >> r2

instance MonadDist Trace where
    -- | @primitive@ handles reuse of @Node@ nodes
    primitive d = Trace $ \db ->
        do
          x <- primitive d
          let newSample = TraceM (Node d x) x
          case db of Node d' x -> return $ reusePrimitive d' x d newSample
                     otherwise -> return newSample

    categorical = error "Can not use Trace on categorical without equality"

-- | Reuse previous sample of primitive RV only if type and parameters match exactly.
-- Whether to reuse a sample is decided in @reusablePrimitive@.
reusePrimitive :: Primitive old -> old -> Primitive new -> TraceM new -> TraceM new
reusePrimitive d old d' new = maybe new (\old' -> TraceM (Node d old) old') (reusablePrimitive d old d')

mhRun :: Trace a -> Int -> Int -> [a]
mhRun = mhRunWithDebugger (const id)

mhRunWithDebugger :: forall a. (RandomDB -> [a] -> [a]) -> Trace a -> Int -> Int -> [a]
mhRunWithDebugger debugger p seed steps =
  if steps <= 0 then
    []
  else
    flip sample (mkStdGen seed) $ do
      initialTrace <- runTrace p None
      otherSamples <- loop initialTrace (steps - 1)
      return (value initialTrace : debugger (randomDB initialTrace) otherSamples)
  where
    loop :: TraceM a -> Int -> Sampler [a]
    -- done
    loop _ steps | steps <= 0 = return []
    -- iterate
    loop old steps = do
      r1 <- update (randomDB old)
      new <- runTrace p r1

      accept <- bernoulli $ acceptanceRatio (randomDB old) (randomDB new)
      let result = if accept then new else old

      otherSamples <- loop result (steps - 1)
      return $  (value result) : debugger (randomDB result) otherSamples

------------------
-- DEBUG TRIALS --
------------------

mhDebug :: Trace Double -> Int -> Int -> [Double]
mhDebug = mhRunWithDebugger (trace . ("  " ++) . show)

-- Run a sampler many times to produce many samples.
-- A reference to check MH against.
sampleMany :: Sampler a -> Int -> Int -> [a]
sampleMany sampler seed size = sample (sequence $ replicate size $ sampler) (mkStdGen seed)

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
    bars = map (\d -> concat $ replicate (min cols $ round (d / ymax * fromIntegral cols)) "#") densities
    annotatedBars = zipWith3 (printf "%5.3f @ %6.3f %s") densities mids bars
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
-- Due to parameter-sample dependency, resampling one value results in
-- resampling all subsequent values in current version of Trace.primitive.
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
--   histogram (Histo (-2.5) 1 33.5 0.25 60) $ mhRun varChoices 0 20000
--   histogram (Histo (-2.5) 1 33.5 0.25 60) $ sampleMany varChoices 0 20000
--
varChoices :: MonadDist m => m Double
varChoices = do
  -- Use Gaussian to mimic geometric, since geometric
  -- is implemented in terms of categorical, and we don't
  -- support categorical yet.
  x <- normal 0 5
  let n = floor (abs x)

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
--   histogram (Histo (-4.25) 0.25 19.25 0.5 60) $ mhRun fig8b 0 30000
--   histogram (Histo (-4.25) 0.25 19.25 0.5 60) $ sampleMany fig8b 0 30000
--
fig8b :: MonadDist m => m Double
fig8b = do
  x <- normal 0 1
  if x > 0 then
    normal 10 2
  else
    return x

-- TODO
--
-- 1. Reformulate Trace as monad transformer
--
-- 2. Validate acceptance ratios by goodness-of-fit tests
--
-- 3. Support `instance MonadBayes (Compose Trace MaybeTupleLogfloat)`
