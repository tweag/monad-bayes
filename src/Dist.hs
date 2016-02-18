{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts
 #-}

module Dist where

import System.Random
import Control.Applicative (Applicative, pure, (<*>))
import Control.Arrow (first, second)
import Control.Monad (liftM, liftM2)
import Data.Number.LogFloat (LogFloat, fromLogFloat, logFloat)
import qualified Data.Number.LogFloat as LogFloat
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Either

import Control.Monad.State.Lazy
import Control.Monad.List
import Control.Monad.Trans.Maybe
import Control.Monad.Identity

import Base


newtype Dist a = Dist (StateT LogFloat [] a)
    deriving (Functor, Applicative, Monad, MonadState LogFloat)

instance MonadDist Dist where
    categorical d = 
        Dist $ StateT $ \s ->
            do
              (x,p) <- Fold.toList d
              return (x, logFloat p * s)
    normal = error "Dist does not support continuous distributions"
    gamma  = error "Dist does not support continuous distributions"
    beta   = error "Dist does not support continuous distributions"

instance MonadBayes Dist where
    factor w = modify (* w)

toList :: Dist a -> [(a,LogFloat)]
toList (Dist d) = runStateT d 1

toCategorical :: Dist a -> [(a,Double)]
toCategorical = map (second fromLogFloat) . toList

enumerate :: Ord a => Dist a -> [(a,Double)]
enumerate d = simplify $ toCategorical d where
    simplify = normalize . compact
    compact = Map.toAscList . Map.fromListWith (+)
    normalize xs = map (second (/ norm)) xs where
        norm = sum (map snd xs)


newtype EmpiricalT m a = EmpiricalT (StateT LogFloat (ListT m) a)
    deriving (Functor, Applicative, Monad, MonadState LogFloat)

runEmpiricalT :: EmpiricalT m a -> StateT LogFloat (ListT m) a
runEmpiricalT (EmpiricalT d) = d

instance MonadTrans EmpiricalT where
    lift = EmpiricalT . lift . lift

instance MonadDist m => MonadDist (EmpiricalT m) where
    categorical = lift . categorical
    normal m s  = lift (normal m s)
    gamma a b   = lift (gamma a b)
    beta a b    = lift (beta a b)

instance MonadDist m => MonadBayes (EmpiricalT m) where
    factor w = modify (* w)

population :: Monad m => Int -> EmpiricalT m ()
population n = EmpiricalT $ lift $ ListT $ sequence $ replicate n $ return ()

fold :: Monad m => (b -> a -> b) -> b -> EmpiricalT m a -> m b
fold f z (EmpiricalT d) = fmap (foldl f z) $ runListT $ evalStateT d 1

all :: Monad m => (a -> Bool) -> EmpiricalT m a -> m Bool
all cond d = fold (\b x -> b && cond x) True d

toCat :: Monad m => EmpiricalT m a -> m [(a,Double)]
toCat (EmpiricalT d) = fmap (map (second fromLogFloat)) $ runListT $ runStateT d 1

resample :: MonadDist m => EmpiricalT m a -> EmpiricalT m a
resample d = do
  cat <- lift $ toCat d
  --let evidence = LogFloat.sum $ map snd cat
  -- EmpiricalT $ modify (* evidence) --to keep track of model evidence
  population (length cat)
  lift $ categorical $ cat

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



data ParticleT m a = ParticleT (m (Either a (ParticleT m a)))
runParticleT (ParticleT m) = m

synchronize :: Monad m => ParticleT m ()
synchronize = ParticleT $ return $ Right $ return ()

flatten :: Monad m => ParticleT m a -> m a
flatten (ParticleT m) = do
  e <- m
  case e of Left x  -> return x
            Right p -> flatten p

instance Functor m => Functor (ParticleT m) where
    fmap f (ParticleT d) = ParticleT $ fmap (emap f) d where
                                            emap f (Left x) = Left (f x)
                                            emap f (Right d) = Right (fmap f d)

instance Monad m => Applicative (ParticleT m) where
    pure = return
    (<*>) = liftM2 ($)

instance Monad m => Monad (ParticleT m) where
    return x = ParticleT $ return $ Left x
    ParticleT d >>= f =
        ParticleT $ do
          p <- d
          case p of Left x  -> runParticleT (f x)
                    Right q -> return $ Right $ q >>= f
    fail = lift . fail

instance MonadTrans ParticleT where
    lift = ParticleT . fmap Left

instance MonadDist m => MonadDist (ParticleT m) where
    categorical = lift . categorical
    normal m s  = lift (normal m s)
    gamma a b   = lift (gamma a b)
    beta a b    = lift (beta a b)

instance MonadBayes m => MonadBayes (ParticleT m) where
    factor w = lift (factor w) >> synchronize


advance :: Monad m => Either a (ParticleT m a) -> m (Either a (ParticleT m a))
advance (Left x)  = return (Left x)
advance (Right p) = runParticleT p


smc :: Int -> ParticleT (EmpiricalT Sampler) a -> EmpiricalT Sampler a
smc n d =
    let
        ParticleT start = lift (population n) >> d
        step particles = resample $ particles >>= advance
        resample = undefined
        run particles = do
          finished <- lift $ Dist.all isLeft particles
          if finished then particles else run (step particles)
    in
      fmap (\(Left x) -> x) $ run start

-- data ParticleT m a = Finished (m a) | Running (m (ParticleT m a))

-- synchronize :: Monad m => ParticleT m ()
-- synchronize = Running (return (return ()))

-- instance Functor m => Functor (ParticleT m) where
--     fmap f (Finished d) = Finished (fmap f d)
--     fmap f (Running d)  = Running (fmap (fmap f) d)

-- instance Monad m => Applicative (ParticleT m) where
--     pure = return
--     (<*>) = liftM2 ($)

-- instance Monad m => Monad (ParticleT m) where
--     return x = Finished (return x)
--     Finished d >>= f = Running (fmap f d)
--     Running  d >>= f = Running (fmap (>>= f) d)

-- instance MonadTrans ParticleT where
--     lift = Finished

-- instance MonadDist m => MonadDist (ParticleT m) where
--     categorical = lift . categorical
--     normal m s  = lift (normal m s)
--     gamma a b   = lift (gamma a b)
--     beta a b    = lift (beta a b)

-- instance MonadBayes m => MonadBayes (ParticleT m) where
--     factor w = lift (factor w) >> synchronize
