{-# LANGUAGE
  GADTs,
  DeriveFunctor
 #-}


module Trace where

import Control.Monad (liftM,liftM2)
import Data.Number.LogFloat
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
  show (Node (Normal m s) x) = printf "Node (Normal %f %f) %f" m s x
  show (Node (Gamma  a b) x) = printf "Node (Gamma %f %f) %f" a b x
  show (Node (Beta   a b) x) = printf "Node (Beta %f %f) %f" a b x

-- consider saving the score for MonadBayes:
--   TraceM a = TraceM RandomDB a LogFloat
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

-- TODO
--
-- 1. Try it out on an example to gain some confidence.
--
-- 2. Support `instance MonadBayes Trace`, since examples of Metropolis-Hastings involve
--    heavy uses of conditioning.
--
-- 3. Make `reusePrimitive` reuse more.
