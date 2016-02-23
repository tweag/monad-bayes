{-# LANGUAGE
  GADTs,
  DeriveFunctor
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

data TraceM a = TraceM RandomDB a
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

-- | The final value produced by the program.
value :: TraceM a -> a
value (TraceM db x) = x

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

-- | If old primitive distribution is equal to the new one, cast the old sample for reuse.
samePrimitive :: Primitive a -> Primitive b -> Maybe (a -> b)
samePrimitive (Normal m s) (Normal m' s') | m == m' && s == s' = Just id
samePrimitive (Gamma  a b) (Gamma  a' b') | a == a' && b == b' = Just id
samePrimitive (Beta   a b) (Beta   a' b') | a == a' && b == b' = Just id
samePrimitive _ _ = Nothing

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
-- Caveat: Always returns 1 in the absence of conditioning.
--
-- Proof. Without conditioning,
--
--   pi(x)  = weight x.
--
-- Recall that
--
--   q(x,y) = C(x,y) * weight (y `minus` x),
--
-- where C(x,y) is the probability of choosing the left-most difference
-- between x and y when updating x. Since the location of update does not
-- depend on the original trace, we have
--
--   q(x,y)/q(y,x) = (C(x,y) * weight (y `minus` x)) / (C(y,x) * weight (x `minus` y))
--                 = weight (y `minus` x) / weight (x `minus` y).
--
-- Observe that for all RandomDB x, y
--
--   y = x  `union` (y `minus` x) `minus` (x `minus` y),
--
-- therefore
--
--   pi(new) = pi(old `union` (new `minus` old) `minus` (old `minus` new))
--           = pi(old) * pi(new `minus` old) / pi(old `minus` new)
--           = pi(old) * q(old, new) / q(new, old).
--
-- If pi(old) * pi(old, new) != 0, then
--
--     (pi(new) * q(new, old)) / (pi(old) * q(old, new))
--   = ((pi(old) * q(old, new) / q(new, old)) * q(new, old)) / (pi(old) * q(old, new))
--   = 1.
--
-- If pi(old) * pi(old, new) == 0, then the acceptance ratio is 1
-- by definition. QED
acceptanceRatio :: RandomDB -> RandomDB -> LogFloat
acceptanceRatio old new =
  let
    pi_old    = weight old
    pi_new    = weight new
    q_old_new = weight (new `minus` old)
    q_new_old = weight (old `minus` new)
  in
    if pi_old * q_old_new == 0 then
      1
    else
      min 1 ((pi_new * q_new_old) / (pi_old * q_old_new))

-- | Updates a trace by resampling a randomly selected site.
-- Caveat: has nonzero probability to leave @RandomDB@ unchanged.
update :: RandomDB -> Sampler RandomDB
update None = return None
update (Node d x) = fmap (Node d) (primitive d)
update (Bind t1 t2) = do
  let p = 0.5
  b <- bernoulli p
  if b then
      do
        t1' <- update t1
        return $ Bind t1' t2
  else 
      do
        t2' <- update t2
        return $ Bind t1 t2'

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
reusePrimitive :: Primitive old -> old -> Primitive new -> TraceM new -> TraceM new
reusePrimitive d old d' new = maybe new (\cast -> TraceM (Node d old) (cast old)) (samePrimitive d d')
-- Consider re-weighing, if d and d' has same type and different distribution/parameters.
-- Care must be taken if old value has density 0 in new distribution,
-- for it could destroy irreducibility.

-- | @mhStep t@ corresponds to the transition kernel of Metropolis-Hastings algorithm
mhStep :: Trace a -> RandomDB -> Sampler (TraceM a)
mhStep p r = do
  -- Single-site mutation of previous stochastic choices.
  r1 <- update r

  -- Re-execute program with new stochastic choices, reusing as much as possible.
  -- If program's 'latent space' is fixed and independent from sampling,
  -- then everything will be reused. After some improvement of @reusePrimitive@,
  -- everything should be reused in the examples of the `joint` branch.
  --
  -- There is no need to compute the acceptance ratio, because it is constant 1
  -- in the absence of conditioning.
  runTrace p r1

mhRun :: Trace Double -> Int -> Int -> [Double]
mhRun = mhRunWithDebugger (const id)

mhRunWithDebugger :: (RandomDB -> [Double] -> [Double]) -> Trace Double -> Int -> Int -> [Double]
mhRunWithDebugger debugger p seed steps = sample (loop None steps) (mkStdGen seed)
  where
    loop :: RandomDB -> Int -> Sampler [Double]
    -- done
    loop r steps | steps <= 0 = return []
    -- iterate
    loop r steps = do
      TraceM r' x' <- mhStep p r
      z <- primitive (Normal 0 1)
      xs <- loop r' (steps - 1)
      return $  x' : debugger r' xs

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
--
-- *Base Trace> mhDebug gaussians 0 10
-- [5.28926406901856  (B (1.753<-N2.000|0.500) (B (3.537<-N3.000|0.500) None))
-- ,5.0841941728211815  (B (1.753<-N2.000|0.500) (B (3.331<-N3.000|0.500) None))
-- ,5.794211633612307  (B (1.753<-N2.000|0.500) (B (4.041<-N3.000|0.500) None))
-- ,6.272594302280648  (B (2.231<-N2.000|0.500) (B (4.041<-N3.000|0.500) None))
-- ,4.434024003979054  (B (2.231<-N2.000|0.500) (B (2.203<-N3.000|0.500) None))
-- ,4.7673406497484905  (B (2.231<-N2.000|0.500) (B (2.536<-N3.000|0.500) None))
-- ,5.975480214428634  (B (2.231<-N2.000|0.500) (B (3.744<-N3.000|0.500) None))
-- ,5.668402928336317  (B (1.924<-N2.000|0.500) (B (3.744<-N3.000|0.500) None))
-- ,4.590407621118976  (B (1.924<-N2.000|0.500) (B (2.666<-N3.000|0.500) None))
-- ,5.235450152376731  (B (2.569<-N2.000|0.500) (B (2.666<-N3.000|0.500) None))
-- ]
gaussians :: MonadDist m => m Double
gaussians = do
  x <- normal 2 0.5
  y <- normal 3 0.5
  return $ x + y

-- Nested do-notation generates left-subtrees.
-- Due to parameter-sample dependency, resampling one value results in
-- resampling all subsequent values in current version of Trace.primitive.
--
-- *Base Trace> mhDebug deps 0 10
-- [0.988922039178331  (B (B (5.000<-N5.000|0.600) (3.904<-G5.000|0.500)) (B (0.989<-B3.904|0.400) None))
-- ,0.8889999749405018  (B (B (5.000<-N5.000|0.600) (3.904<-G5.000|0.500)) (B (0.889<-B3.904|0.400) None))
-- ,0.9821310462936671  (B (B (5.000<-N5.000|0.600) (3.904<-G5.000|0.500)) (B (0.982<-B3.904|0.400) None))
-- ,0.8997361173964787  (B (B (5.476<-N5.000|0.600) (2.563<-G5.476|0.500)) (B (0.900<-B2.563|0.400) None))
-- ,0.8544760888723296  (B (B (5.476<-N5.000|0.600) (2.563<-G5.476|0.500)) (B (0.854<-B2.563|0.400) None))
-- ,0.5041173297233119  (B (B (5.476<-N5.000|0.600) (2.563<-G5.476|0.500)) (B (0.504<-B2.563|0.400) None))
-- ,0.9911494761744635  (B (B (5.476<-N5.000|0.600) (2.563<-G5.476|0.500)) (B (0.991<-B2.563|0.400) None))
-- ,0.8981134404494058  (B (B (4.849<-N5.000|0.600) (1.738<-G4.849|0.500)) (B (0.898<-B1.738|0.400) None))
-- ,0.9062046452550016  (B (B (4.849<-N5.000|0.600) (1.738<-G4.849|0.500)) (B (0.906<-B1.738|0.400) None))
-- ,0.9978287490830436  (B (B (4.144<-N5.000|0.600) (3.480<-G4.144|0.500)) (B (0.998<-B3.480|0.400) None))
-- ]
deps :: MonadDist m => m Double
deps = do
  x <- do
    z <- normal 5 0.6
    gamma z 0.5
  y <- beta x 0.4
  return y

-- TODO
--
-- 0. Try MH on fig 2 of Hur, Nori, Rajamani, Samuel.
--
-- 1. Try an example where sampling affects the number of primitive values
--
-- 2. Support `instance MonadBayes (Compose Trace MaybeTupleLogfloat)`
--
-- 3. Make `reusePrimitive` reuse more.
