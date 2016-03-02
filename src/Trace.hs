{-# LANGUAGE
  GADTs,
  DeriveFunctor,
  ScopedTypeVariables
 #-}


module Trace where

import Control.Monad (liftM, liftM2, mplus)
import Control.Monad.State.Lazy
import Data.Maybe (isJust, fromJust)
import Data.List (unfoldr, intercalate)
import Data.Typeable
import Data.Number.LogFloat hiding (sum)
import System.Random
import Text.Printf

import Debug.Trace

import Primitive
import Base
import Dist (normalize)
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
  show (Node (Normal m s) x) = printf "[%.3f<-N%.3f|%.3f]" x m s
  show (Node (Gamma  a b) x) = printf "[%.3f<-G%.3f|%.3f]" x a b
  show (Node (Beta   a b) x) = printf "[%.3f<-B%.3f|%.3f]" x a b
  show (Bind t1 t2) = printf "(B %s %s)" (show t1) (show t2)
  -- print Booleans for debugging
  show (Node (Categorical ps) x) =
    case cast (x, ps) :: Maybe (Bool, [(Bool, LogFloat)]) of
      Nothing      -> "[cat(" ++ (tail $ init $ show $ map snd ps) ++ ")]"
      Just (x, ps) -> "[" ++ show x ++ "<-(" ++
                         (intercalate "," $ map (\(b,p) -> head (show b) : printf "%.3f" (fromLogFloat p)) ps)
                         ++ ")]"

data TraceM a = TraceM { randomDB :: RandomDB, density :: LogFloat, value :: a }
    deriving (Functor)

-- the monad instance below is used in `instance Monad Trace`
instance Applicative TraceM where
    pure = TraceM None 1
    (<*>) = liftM2 ($)

instance Monad TraceM where
    (TraceM rx px x) >>= f =
        let
          TraceM ry py y = f x
        in
          TraceM (Bind rx ry) (px * py) y

-- | The number of random choices in the RandomDB.
size :: RandomDB -> Int
size None = 0
size (Node _ _) = 1
size (Bind t1 t2) = size t1 + size t2

-- | The number of random choices we can choose to mutate
-- in one step of Metropolis-Hastings.
--
-- Due to the necessity of reverse-jump MCMC correction,
-- the likelihood of a proposal must be computable from
-- the trace alone. Therefore we mutate a stochastic choice
-- only if it is not Dirac distributed, and always propose
-- a value distinct from the previous sample.
proposalDimension :: RandomDB -> Int
proposalDimension (Node d x) | proposablePrimitive d x = 1
proposalDimension (Bind t1 t2) = proposalDimension t1 + proposalDimension t2
proposalDimension _ = 0

-- | Return whether a primitive stochastic choice contributes
-- to the proposal space.
proposablePrimitive :: forall a. Primitive a -> a -> Bool
proposablePrimitive d x = isJust $ proposalPrimitive d x

-- | If a primitive distribution is not Dirac, compute the
-- proposal distribution obtained by setting the density
-- of the old sample to 0.
--
-- It is a bit inaccurate on continuous random variables
-- because the old value can be resampled with a very small
-- probability.
proposalPrimitive :: forall a. Primitive a -> a -> Maybe (Primitive a)
proposalPrimitive (Categorical d) x =
  if length (filter (\pair -> snd pair > 0) d) <= 1 then
    -- The categorical distribution is Dirac;
    -- this primitive is not in the proposal space.
    Nothing
  else
    -- This primitive is in the proposal space.
    -- The proposal distribution does not produce the old sample.
    Just $ Categorical $ normalize (filter (\pair -> fst pair /= x) d)
proposalPrimitive d x = fmap (const d) $ isPrimitiveDouble d

-- | The product of all densities in the trace.
weight :: RandomDB -> LogFloat
weight None = 1
weight (Node d x) = pdf d x
weight (Bind t1 t2) = weight t1 * weight t2

-- | An old primitive sample is reusable if both distributions have the
-- same type and if old sample does not decrease acceptance ratio by
-- more than a threshold.
reusablePrimitive :: Primitive a -> a -> Primitive b -> Maybe ((b, LogFloat), a -> Bool)
reusablePrimitive d x d' =
  let
    threshold = 0.0
  in
    reusablePrimitiveDouble threshold d x d' `mplus` reusableCategorical threshold d x d'

-- | Try to reuse a sample from a categorical distribution
-- if it does not decrease acceptance ration by more than a threshold.
reusableCategorical :: LogFloat -> Primitive a -> a -> Primitive b -> Maybe ((b, LogFloat), a -> Bool)
reusableCategorical threshold d@(Categorical _) x d'@(Categorical _) = do
  x' <- cast x
  let pOld = pdf d x
  let pNew = pdf d' x'
  if pOld > 0 && pNew / pOld > threshold then
    Just ((x', pNew), (== x') . fromJust . cast)
  else
    Nothing
reusableCategorical threshold _ _ _ = Nothing

-- | An old primitive sample of type Double is reused if old sample does
-- not decrease acceptance ratio by more than a threshold.
-- In particular, a sample is always reused if its distribution did not
-- change, since it does not decrease acceptance ratio at all.
reusablePrimitiveDouble :: LogFloat -> Primitive a -> a -> Primitive b -> Maybe ((b, LogFloat), a -> Bool)
reusablePrimitiveDouble threshold d x d' =
  case (isPrimitiveDouble d, isPrimitiveDouble d') of
    (Just (from, to), Just (from', to')) ->
      let
        x' = from' $ to x
        pOld = pdf d x
        pNew = pdf d' x'
      in
        if pOld > 0 && pNew / pOld > threshold then
          Just ((x', pNew), (== to' x') . to)
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
    Just (_, isReused) -> isReused x
    Nothing      -> False

-- | Compute the element in the proposal space that mutated the first
-- RandomDB into the second RandomDB.
--
-- In order to make reversible-jump MCMC correction possible,
-- we have to assume that all sources of nondeterministic execution
-- are recorded in RandomDB. The assumption translates to a generalized
-- prefix-free property: If two traces differ, then their left-most
-- difference is between two `Node` with the same distribution and
-- different samples.
minus :: RandomDB -> RandomDB -> RandomDB
minus new0 old0 = evalState (loop new0 old0) True
  where
    loop :: RandomDB -> RandomDB -> State Bool RandomDB

    loop None None = return None

    loop (Bind lNew rNew) (Bind lOld rOld) = do
      l <- loop lNew lOld
      r <- loop rNew rOld
      return $ Bind l r

    loop (Node d' x') (Node d x) = do
      leftMost <- get
      if leftMost then
        do
          case (compareNodes d' x' d x, proposalPrimitive d x) of

            -- Node is reused. The left-most modified node is
            -- further to the right.
            (Just (True, _), _) -> return None

            -- Node is the left-most modified one.
            -- Compute the proposal distribution and modify state.
            (Just (False, cast), Just q) ->
              do
                put False
                return $ Node q (cast x')

            -- Node is the left-most modified one,
            -- but the prefix-free property is not satisfied.
            otherwise -> prefixFreePropertyViolated

      else
        -- Leftmost modification is already done
        if reusedSample d x d' x' then
          return None
        else
          return $ Node d' x'

    loop new old = do
      leftMost <- get
      if leftMost then
        prefixFreePropertyViolated
      else
        return new

    -- Compute whether two nodes have the same distribution.
    -- If they do, return whether the two samples are identical
    -- and the safe cast between their types.
    --
    -- Categorical distributions are compared by ==, which is a bit
    -- too conservative in general. It's good enough for the trace
    -- mutation strategy in this file, because the distribution of
    -- the selected update site is copied directly into the new trace.
    compareNodes :: Primitive a -> a -> Primitive b -> b -> Maybe (Bool, a -> b)
    compareNodes (Normal m s) x (Normal m' s') x' | m == m' && s == s' = Just (x == x', id)
    compareNodes (Gamma  a b) x (Gamma  a' b') x' | a == a' && b == b' = Just (x == x', id)
    compareNodes (Beta   a b) x (Beta   a' b') x' | a == a' && b == b' = Just (x == x', id)
    compareNodes (Categorical d) x (Categorical d') x' | cast d == Just d' =
      Just (cast x == Just x', fromJust . cast)
    compareNodes _ _ _ _ = Nothing

    -- Throw an error in case prefix-free property is violated
    prefixFreePropertyViolated =
      error $ "Prefix-free property violated by the following traces:\n\n" ++
          "Old RandomDB =\n" ++ show old0 ++ "\n\n" ++
          "New RandomDB =\n" ++ show new0 ++ "\n"

-- | From two @RandomDB@, compute the acceptance ratio.
-- Precondition: Program is @MonadDist@ but not @MonadBayes@
--               (i.e., it doesn't call @condition@ or @factor@),
--               so that the target density of an execution is completely
--               determined by the stochastic choices made.
acceptanceRatio :: TraceM a -> TraceM a -> LogFloat
acceptanceRatio old new =
  let
    rOld      = randomDB old
    rNew      = randomDB new
    size_old  = proposalDimension rOld
    size_new  = proposalDimension rNew
    pi_old    = density old
    pi_new    = density new
    q_old_new = weight (rNew `minus` rOld) / fromIntegral size_old
    q_new_old = weight (rOld `minus` rNew) / fromIntegral size_new

    numerator   = pi_new * q_new_old
    denominator = pi_old * q_old_new
  in
    if size_old * size_new == 0 || denominator == 0 then
      1
    else
      min 1 (numerator / denominator)

-- | Resample the i-th random choice in the proposal space
-- from the proposal distribution.
updateAt :: Int -> RandomDB -> Sampler RandomDB
updateAt n db =
  fmap (either id $ error $ printf "updateAt: index %d out of bound in %s\n" n (show db)) (loop n db)
  where
    -- Return either an updated @RandomDB@
    --            or the result of subtracting the number of traversed
    --               random choices from the index
    loop :: Int -> RandomDB -> Sampler (Either RandomDB Int)
    loop n None = return $ Right n
    loop n (Node d x) | n == 0 =
      case proposalPrimitive d x of
        Just q  -> fmap (Left . Node d) (primitive q) -- note that d /= q
        Nothing -> return $ Right n
    loop n (Node d x) = return $ Right (n - 1)
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
  let n = proposalDimension r
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
          let newSample = TraceM (Node d x) (pdf d x) x
          case db of Node d' x -> return $ reusePrimitive d' x d newSample
                     otherwise -> return newSample

    categorical = primitive . Categorical

instance MonadBayes Trace where
  condition True  = return ()
  condition False = factor 0 -- Assume rejected trace don't crash.
  factor p = Trace $ const . return $ TraceM None p ()

-- | Try to reuse previous sample of primitive random variable.
-- Whether to reuse a sample is decided in @reusablePrimitive@.
reusePrimitive :: Primitive old -> old -> Primitive new -> TraceM new -> TraceM new
reusePrimitive d old d' new =
  maybe new (\(old', p') -> TraceM (Node d' old') p' old') (fmap fst $ reusablePrimitive d old d')

data Stat = Stat
  { accepted  :: Bool
  , ratio     :: LogFloat
  , oldSize   :: Int
  , newSize   :: Int
  , resampled :: Int
  }

mhRun :: Trace a -> Int -> Int -> [a]
mhRun p seed steps = fst $ mhRunWithDebugger (const id) p seed steps

mhRunWithDebugger :: forall a. (RandomDB -> [a] -> [a]) -> Trace a -> Int -> Int -> ([a], [Stat])
mhRunWithDebugger debugger p seed steps =
  if steps <= 0 then
    ([], [])
  else
    flip sample (mkStdGen seed) $ do
      initialTrace <- runTrace p None
      -- discards first result
      loop initialTrace (steps - 1)
  where
    loop :: TraceM a -> Int -> Sampler ([a], [Stat])
    -- done
    loop _ steps | steps <= 0 = return ([], [])
    -- iterate
    loop old steps = do
      r1 <- update (randomDB old)
      new <- runTrace p r1

      let ratio = acceptanceRatio old new
      accept <- bernoulli ratio
      let result = if accept then new else old

      (otherSamples, otherStats) <- loop result (steps - 1)

      let samples = (value result) : debugger (randomDB result) otherSamples

      let oldSize   = size (randomDB old)
      let newSize   = size (randomDB new)
      let resampled = size (randomDB new `minus` randomDB old)
      let stat      = Stat accept ratio oldSize newSize resampled

      return (samples, stat : otherStats)

------------------
-- DEBUG TRIALS --
------------------

mhDebug :: Trace Double -> Int -> Int -> [Double]
mhDebug p seed size = fst $ mhRunWithDebugger (trace . ("  " ++) . show) p seed size

-- Run a sampler many times to produce many samples.
-- A reference to check MH against.
sampleMany :: Sampler a -> Int -> Int -> [a]
sampleMany sampler seed size = sample (sequence $ replicate size $ sampler) (mkStdGen seed)

mhDebugHistogram :: Histo -> Trace Double -> Int -> Int -> IO ()
mhDebugHistogram histo p seed steps = do
  let (samples, stats) = mhRunWithDebugger (const id) p seed steps

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
  let totalResampled = sum $ map resampled stats
  let totalReused = totalNewSize - totalResampled
  let reuseRate = fromIntegral totalReused / fromIntegral totalNewSize :: Double
  putStrLn $ printf "Reuse rate = %04.2f %%" (100 * reuseRate)
  histogram rateHisto (map (\s -> 1 - fromIntegral (resampled s) / fromIntegral (newSize s)) stats)


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
--   mhDebugHistogram (Histo (-4.25) 0.25 19.25 0.5 60) fig8b 0 30000
--   histogram (Histo (-4.25) 0.25 19.25 0.5 60) $ sampleMany fig8b 0 30000
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
--   mhDebugHistogram (Histo (-0.5) 1.0 1.5 1.0 60) grassModel 0 20000
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

-- TODO
--
-- 0. Decompose Trace into monad combinators?
--
-- 1. Reformulate Trace as monad transformer.
--
-- 2. Validate acceptance ratios by goodness-of-fit tests
