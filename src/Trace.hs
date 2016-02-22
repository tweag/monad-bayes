{-# LANGUAGE
  GADTs
 #-}


module Trace where

import Control.Monad (liftM,liftM2)
import Data.Number.LogFloat

import Primitive
import Base
import Sampler

-- | A random database of stochastic choices made in the program.
data RandomDB a where
    Pure :: a -> RandomDB a
    Node :: Primitive a -> a -> RandomDB a
    -- The arguments to Bind are: trace to this point, trace from this point,
    -- continuation that can be used to generate new trace if the first part changes.
    Bind :: RandomDB b -> RandomDB a -> (b -> Trace a) -> RandomDB a 

-- The three instances below are probably useless.
instance Functor RandomDB where
    fmap f (Pure x) = Pure (f x)
    fmap f d = liftM f d

instance Applicative RandomDB where
    pure = Pure
    (<*>) = liftM2 ($)

instance Monad RandomDB where
    return = pure
    d >>= f = Bind d (f (value d)) (Trace . return . f)

-- | The final value produced by the program.
value :: RandomDB a -> a
value (Pure x) = x
value (Node d x) = x
value (Bind t1 t2 k) = value t2

-- | The number of random choices in the RandomDB.
size :: RandomDB a -> Int
size (Pure _) = 0
size (Node _ _) = 1
size (Bind t1 t2 _) = size t1 + size t2

-- | The product of all densities in the trace.
weight :: RandomDB a -> LogFloat
weight (Pure _) = 1
weight (Node d x) = pdf d x
weight (Bind t1 t2 _) = weight t1 * weight t2

-- | Updates a trace by resampling a randomly selected site.
update :: RandomDB a -> Sampler (RandomDB a)
update (Pure x) = return $ Pure x
update (Node d x) = fmap (Node d) (primitive d)
update (Bind t1 t2 k) = do
  let p = 0.5
  b <- bernoulli p
  if b then 
      do
        t1' <- update t1
        t2' <- runTrace $ k $ value t1'
        return $ Bind t1' t2' k
  else 
      do
        t2' <- update t2
        return $ Bind t1 t2' k

-- | Reuses the random choices from the first computation in the second one.
reuse :: RandomDB a -> RandomDB a -> Trace a
reuse = undefined

newtype Trace a = Trace (Sampler (RandomDB a))
runTrace (Trace d) = d

instance Functor Trace where
    fmap = liftM

instance Applicative Trace where
    pure = return
    (<*>) = liftM2 ($)

instance Monad Trace where
    return = Trace . return . return
    t >>= f = Trace $
        do
          let d = runTrace t
          r1 <- d
          let x = value r1
          r2 <- runTrace (f x)
          return $ Bind r1 r2 f where

instance MonadDist Trace where
    primitive d = Trace $ do
          x <- primitive d
          return $ Node d x
    categorical = error "Can not use Trace on categorical without equality"


mhStep :: RandomDB a -> Sampler (RandomDB a)
mhStep r = do
  r' <- update r
  let ratio = weight r' * fromIntegral (size r') / (weight r * fromIntegral (size r))
  accept <- bernoulli ratio
  return $ if accept then r' else r
