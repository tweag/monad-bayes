{-# LANGUAGE
  GeneralizedNewtypeDeriving
 #-}

module Importance (
    Weight,
    weight,
    unWeight,
    ImportanceT(ImportanceT),  --constructor is needed in Dist
    runImportanceT
                  ) where

import Control.Arrow (first,second)
import Data.Number.LogFloat
import Data.Monoid
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import Base

-- | Representation of a weight in importance sampling algorithms.
-- Internally represented in log-domain, but semantically a non-negative real number.
-- 'Monoid' instance with respect to multiplication.
newtype Weight = Weight (Product LogFloat)
    deriving(Eq, Num, Ord, Show, Monoid)

weight :: LogFloat -> Weight
weight = Weight . Product

unWeight :: Weight -> LogFloat
unWeight (Weight (Product p)) = p

-- | A wrapper for 'WriterT' 'Weight' that executes the program
-- emitting the likelihood score.
newtype ImportanceT m a = ImportanceT {toWriterT :: WriterT Weight m a}
    deriving(Functor, Applicative, Monad, MonadTrans, MonadDist)

runImportanceT :: Functor m => ImportanceT m a -> m (a, LogFloat)
runImportanceT = fmap (second unWeight) . runWriterT . toWriterT

instance MonadDist m => MonadBayes (ImportanceT m) where
    factor = ImportanceT . tell . weight
