{-# LANGUAGE
  GADTs,
  DeriveFunctor
 #-}


module Trace where

import Control.Monad (liftM,liftM2)
import Data.Number.LogFloat
import System.Random
import Text.Printf

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
-- Caution: Without conditioning, acceptance ratio is always 1.
--
-- Proof. Without conditioning,
--
--   pi(x)  = weight x.
--
-- Recall that
--
--   q(x,y) = weight (y `minus` x).
--
-- Since for all RandomDB x, y
--
--   y = x  `union` (y `minus` x) `minus` (x `minus` y),
--
-- we have
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

-- | From two @RandomDB@, compute the acceptance ratio.
-- Precondition: Program is @MonadDist@ but not @MonadBayes@
--               (i.e., it doesn't call @condition@ or @factor@),
--               so that the target density of an execution is completely
--               determined by the stochastic choices made.
--
-- Caveat: Always returns 1. See comment after the definition of @minus@.
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
update :: RandomDB -> Sampler RandomDB
update None = return None
update (Node d x) = fmap (Node d) (primitive d)
update (Bind t1 t2) = do
  let p = 0.5
  b <- bernoulli p
  if b || size t2 == 0 then
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

    -- | >>= handles reuse of @Bind@ nodes
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
mhStep :: Trace a -> TraceM a -> Sampler (TraceM a)
mhStep (Trace p) (TraceM r x) = do
  -- Single-site mutation of previous stochastic choices.
  r1 <- update r

  -- Re-execute program with new stochastic choices, reusing as much as possible.
  -- If program's 'latent space' is fixed and independent from sampling,
  -- then everything will be reused. After some improvement of @reusePrimitive@,
  -- everything should be reused in the examples of the `joint` branch.
  --
  TraceM r' x' <- p r1
  let ratio = acceptanceRatio r r'
  accept <- bernoulli ratio
  return $ if accept then TraceM r' x' else TraceM r x

------------------
-- DEBUG TRIALS --
------------------

mhDebug :: Trace Double -> Int -> Int -> IO [Double]
mhDebug (Trace p) seed steps = loop Nothing (mkStdGen seed) steps
  where
    loop :: Maybe (TraceM Double) -> StdGen -> Int -> IO [Double]
    -- done
    loop r gen steps | steps <= 0 = return []
    -- initialize
    loop Nothing gen steps = do
      let (gen1, gen2) = split gen
      let TraceM r' x' = flip sample gen1 $ p None
      putStrLn $ printf "    %.3f %s" x' (show r')
      xs <- loop (Just $ TraceM r' x') gen2 (steps - 1)
      return $ x' : xs
    -- iterate
    loop (Just tr@(TraceM r x)) gen steps = do
      let (gen1, gen2) = split gen
      let (newTrace@(TraceM r' x'), accept, pi_old, pi_new, q_old_new, q_new_old, rNew) = flip sample gen1 $ do {
          r1 <- update r
        ; TraceM r' x' <- p r1

        ; let old       = r
        ; let new       = r'
        ; let pi_old    = weight old
        ; let pi_new    = weight new
        ; let q_old_new = weight (new `minus` old)
        ; let q_new_old = weight (old `minus` new)
        ; let ratio     = acceptanceRatio old new

        ; accept <- bernoulli ratio
        ; return (if accept then TraceM r' x' else tr, accept, pi_old, pi_new, q_old_new, q_new_old, r')
        }

      -- debug acceptance ratio
      putStrLn ""
      putStrLn $ "old DB = " ++ show r
      putStrLn $ "new DB = " ++ show rNew
      putStr $ printf "pi_new = %.3f * " (fromLogFloat pi_new)
      putStr $ printf "q_new_old = %.3f / " (fromLogFloat q_new_old)
      putStr $ printf "pi_old = %.3f * " (fromLogFloat pi_old)
      putStr $ printf "q_old_new = %.3f" (fromLogFloat q_old_new)
      putStrLn ""
      putStrLn $ printf "acceptance ratio = %.3f" (fromLogFloat $ acceptanceRatio r rNew)
      putStrLn $ printf "new `minus` old = %s" (show $ rNew `minus` r)
      putStrLn $ printf "old `minus` new = %s" (show $ r `minus` rNew)

      let acc = if accept then "acc" else "rej"
      putStrLn $ printf "%s %.3f %s %.3f %.3f" acc x' (show r') (fromLogFloat $ weight r) (fromLogFloat $ weight r')
      xs <- loop (Just newTrace) gen2 (steps - 1)
      return $ x' : xs

-- Successive lines in do-notation generates right-skewed trace.
--
-- *Base Trace> mhDebug gaussians 0 10
--     4.445 (B (1.445<-N2.000|0.500) (B (3.000<-N3.000|0.500) None))
-- rej 4.445 (B (1.445<-N2.000|0.500) (B (3.000<-N3.000|0.500) None))
-- acc 5.298 (B (2.298<-N2.000|0.500) (B (3.000<-N3.000|0.500) None))
-- rej 5.298 (B (2.298<-N2.000|0.500) (B (3.000<-N3.000|0.500) None))
-- rej 5.298 (B (2.298<-N2.000|0.500) (B (3.000<-N3.000|0.500) None))
-- acc 5.400 (B (2.298<-N2.000|0.500) (B (3.103<-N3.000|0.500) None))
-- acc 4.853 (B (2.298<-N2.000|0.500) (B (2.555<-N3.000|0.500) None))
-- rej 4.853 (B (2.298<-N2.000|0.500) (B (2.555<-N3.000|0.500) None))
-- acc 5.093 (B (2.298<-N2.000|0.500) (B (2.795<-N3.000|0.500) None))
-- rej 5.093 (B (2.298<-N2.000|0.500) (B (2.795<-N3.000|0.500) None))
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
--     0.991 (B (B (4.540<-N5.000|0.600) (3.784<-G4.540|0.500)) (B (0.991<-B3.784|0.400) None))
-- rej 0.991 (B (B (4.540<-N5.000|0.600) (3.784<-G4.540|0.500)) (B (0.991<-B3.784|0.400) None))
-- acc 0.975 (B (B (4.387<-N5.000|0.600) (2.177<-G4.387|0.500)) (B (0.975<-B2.177|0.400) None))
-- rej 0.975 (B (B (4.387<-N5.000|0.600) (2.177<-G4.387|0.500)) (B (0.975<-B2.177|0.400) None))
-- rej 0.975 (B (B (4.387<-N5.000|0.600) (2.177<-G4.387|0.500)) (B (0.975<-B2.177|0.400) None))
-- acc 0.841 (B (B (4.387<-N5.000|0.600) (2.177<-G4.387|0.500)) (B (0.841<-B2.177|0.400) None))
-- acc 0.823 (B (B (4.387<-N5.000|0.600) (2.177<-G4.387|0.500)) (B (0.823<-B2.177|0.400) None))
-- rej 0.823 (B (B (4.387<-N5.000|0.600) (2.177<-G4.387|0.500)) (B (0.823<-B2.177|0.400) None))
-- acc 0.989 (B (B (4.387<-N5.000|0.600) (2.177<-G4.387|0.500)) (B (0.989<-B2.177|0.400) None))
-- rej 0.989 (B (B (4.387<-N5.000|0.600) (2.177<-G4.387|0.500)) (B (0.989<-B2.177|0.400) None))
deps :: MonadDist m => m Double
deps = do
  x <- do
    z <- normal 5 0.6
    gamma z 0.5
  y <- beta x 0.4
  return y

-- TODO
--
-- 0. Cleanup debug code using `Debug.Trace.trace`, correct trial comment
--
-- 1. Try an example where sampling affects the number of primitive values
--
-- 2. Support `instance MonadBayes (Compose Trace MaybeTupleLogfloat)`
--
-- 3. Make `reusePrimitive` reuse more.
