{-# LANGUAGE
  GADTs
 #-}


module Trace where

import Control.Monad (liftM,liftM2)
import Data.Number.LogFloat
import qualified Data.Map.Lazy as Map
import qualified Data.Foldable as Fold

import Primitive
import Base
import Sampler

type DistType = () -- repeat constructors of Primitive
type Label = () -- in the simplest case this is just unit

newtype RandomDB = RandomDB (Map.Map DistType [(Primitive a, a)])

record :: Primitive a -> a -> RandomDB
record = undefined

lookup :: RandomDB -> Primitive a -> Maybe a
lookup = undefined

instance Monoid RandomDB where
  mzero = RandomDB empty
  mplus (RandomDB a) (RandomDB b) = RandomDB $ unionWith (++) a b

instance Monoid LogFloat where
  mempty = 1
  mappend = (*)

weight :: RandomDB -> LogFloat
weight = Fold.foldMap f where
  f xs = Fold.foldMap (uncurry pdf)

update :: MonadDist m => MHKernel m RandomDB
update = undefined


newtype TraceT m a = TraceT {runTraceT :: WriterT RandomDB m a}
                   deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter)

instance MonadDist m => MonadDist (TraceT m) where
  primitive d = do
    x <- lift (primitive d)
    tell (record d x)
    return x


newtype ReuserT m a = ReuserT {runReuserT :: ReaderT RandomDB m a}
                      deriving (Functor, Applicative, Monad, MonadTrans, MonadReader)

instance MonadDist m => MonadDist (ReuserT m) where
  primitive d = do
    rdb <- ask
    case lookup rdb d of Just x -> return x
                         Nohing -> lift (primitive d)
    -- need some sort of update to remove the read value from the database
    -- maybe use StateT instead of ReaderT?
