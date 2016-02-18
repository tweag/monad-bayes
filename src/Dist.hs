{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances
 #-}

module Dist where

import System.Random
import Control.Applicative (Applicative, pure, (<*>))
import Control.Arrow (first, second)
import Control.Monad (liftM, liftM2)
import Data.Number.LogFloat (LogFloat, fromLogFloat, logFloat)
import Data.Foldable (toList)
import qualified Data.Map as Map

import Control.Monad.State.Lazy
import Control.Monad.List
import Control.Monad.Trans.Maybe

import Base


newtype Dist a = Dist (StateT LogFloat [] a)
    deriving (Functor, Applicative, Monad, MonadState LogFloat)

instance MonadDist Dist where
    categorical d = 
        Dist $ StateT $ \s ->
            do
              (x,p) <- toList d
              return (x, logFloat p * s)
    normal = error "Dist does not support continuous distributions"
    gamma  = error "Dist does not support continuous distributions"
    beta   = error "Dist does not support continuous distributions"

instance MonadBayes Dist where
    factor w = modify (* w)

enumerate :: Ord a => Dist a -> [(a,Double)]
enumerate (Dist d) = simplify (runStateT d 1) where
    simplify = normalize . compact
    compact = Map.toAscList . Map.fromListWith (+) . map (second fromLogFloat)
    normalize xs = map (second (/ norm)) xs where
        norm = sum (map snd xs)


newtype Sampler a = Sampler (StdGen -> a)
    deriving (Functor)

instance Applicative Sampler where
    pure = return
    (<*>) = liftM2 ($)

instance Monad Sampler where
    return = Sampler . const
    Sampler d >>= f = Sampler $
                       \g -> let
                           x          = d g1
                           Sampler y  = f x
                           (g1,g2)    = split g
         in
           y g2

instance MonadDist Sampler where
    categorical = undefined
    normal = undefined
    gamma = undefined
    beta = undefined



instance MonadDist m => MonadDist (MaybeT m) where
    categorical = lift . categorical
    normal m s  = lift (normal m s)
    gamma a b   = lift (gamma a b)
    beta a b    = lift (beta a b)

instance MonadDist m => MonadBayes (MaybeT m) where
    factor = undefined

rejection :: MaybeT Sampler a -> Sampler a
rejection d = do
  m <- runMaybeT d
  case m of Just x  -> return x
            Nothing -> rejection d



instance MonadDist m => MonadDist (StateT s m) where
    categorical = lift . categorical
    normal m s  = lift (normal m s)
    gamma a b   = lift (gamma a b)
    beta a b    = lift (beta a b)

instance MonadDist m => MonadBayes (StateT LogFloat m) where
    factor w = modify (* w)

importance :: StateT LogFloat Sampler a -> Sampler (a,LogFloat)
importance d = runStateT d 1





data ParticleT m a = Finished (m a) | Running (m (ParticleT m a))

synchronize :: Monad m => ParticleT m ()
synchronize = Running (return (return ()))

instance Functor m => Functor (ParticleT m) where
    fmap f (Finished d) = Finished (fmap f d)
    fmap f (Running d)  = Running (fmap (fmap f) d)

instance Monad m => Applicative (ParticleT m) where
    pure = return
    (<*>) = liftM2 ($)

instance Monad m => Monad (ParticleT m) where
    return x = Finished (return x)
    Finished d >>= f = Running (fmap f d)
    Running  d >>= f = Running (fmap (>>= f) d)

instance MonadTrans ParticleT where
    lift = Finished

instance MonadDist m => MonadDist (ParticleT m) where
    categorical = lift . categorical
    normal m s  = lift (normal m s)
    gamma a b   = lift (gamma a b)
    beta a b    = lift (beta a b)

instance MonadBayes m => MonadBayes (ParticleT m) where
    factor w = lift (factor w) >> synchronize
