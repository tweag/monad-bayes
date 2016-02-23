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
-- is there any stylistic reason for this instead of `newtype Trace a = Trace { runTrace :: ... }`?

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
reusePrimitive d@(Normal m s) old (Normal m' s') new | m == m' && s == s' = TraceM (Node d old) old
reusePrimitive d@(Gamma  a b) old (Gamma  a' b') new | a == a' && b == b' = TraceM (Node d old) old
reusePrimitive d@(Beta   a b) old (Beta   a' b') new | a == a' && b == b' = TraceM (Node d old) old
reusePrimitive _ _ _ new = new
-- Consider re-weighing instead, if weight of previous sample is high enough according to new distribution.
-- Problem: How can we let the user configure the weight threshold for reuse?

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

  -- Compute some provisional acceptance ratio
  let ratio = weight r' * fromIntegral (size r') / (weight r * fromIntegral (size r))

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
      let (newTrace@(TraceM r' x'), accept) = flip sample gen1 $ do {
          r1 <- update r
        ; TraceM r' x' <- p r1
        ; let ratio = 0.5 -- acceptance ratio is wrong anyway
        ; accept <- bernoulli ratio
        ; return (if accept then TraceM r' x' else TraceM r x, accept)
        }
      let acc = if accept then "acc" else "rej"
      putStrLn $ printf "%s %.3f %s" acc x' (show r')
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
-- Due to parameter-sample dependency, resampling one Gaussian results in
-- resampling all subsequent Gaussians in current version of Trace.primitive.
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
-- 1. Refactor to compute acceptance ratio.
--
-- 2. Support `instance MonadBayes (Compose Trace MaybeTupleLogfloat)`
--
-- 3. Make `reusePrimitive` reuse more.
