{-# LANGUAGE StandaloneDeriving #-}

module Control.Applicative.List where

-- base
import Control.Applicative
-- transformers
import Control.Monad.Trans.Writer.Strict
import Data.Functor.Compose
-- log-domain
import Numeric.Log (Log)

-- * Applicative ListT

-- | _Applicative_ transformer adding a list/nondeterminism/choice effect.
--   It is not a valid monad transformer, but it is a valid 'Applicative'.
newtype ListT m a = ListT {getListT :: Compose m [] a}
  deriving newtype (Functor, Applicative, Alternative)

lift :: (Functor m) => m a -> ListT m a
lift = ListT . Compose . fmap pure

runListT :: ListT m a -> m [a]
runListT = getCompose . getListT

-- * Applicative Population transformer

-- WriterT has to be used instead of WeightedT,
-- since WeightedT uses StateT under the hood,
-- which requires a Monad (ListT m) constraint.
newtype PopulationT m a = PopulationT {getPopulationT :: WriterT (Log Double) (ListT m) a}
  deriving newtype (Functor, Applicative, Alternative)

runPopulationT :: PopulationT m a -> m [(a, Log Double)]
runPopulationT = runListT . runWriterT . getPopulationT

fromWeightedList :: m [(a, Log Double)] -> PopulationT m a
fromWeightedList = PopulationT . WriterT . ListT . Compose
