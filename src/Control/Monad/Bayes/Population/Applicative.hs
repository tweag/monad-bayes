-- | 'PopulationT' turns a single sample into a collection of weighted samples.
--
-- This module contains an _'Applicative'_ transformer corresponding to the Population monad transformer from the article.
-- It is based on the old-fashioned 'ListT', which is not a valid monad transformer, but a valid applicative transformer.
-- The corresponding monad transformer is contained in 'Control.Monad.Bayes.Population'.
-- One can convert from the monad transformer to the applicative transformer by 'flatten'ing.
module Control.Monad.Bayes.Population.Applicative where

import Control.Applicative
import Control.Applicative.List
import Control.Monad.Trans.Writer.Strict
import Data.Functor.Compose
import Numeric.Log (Log)

-- * Applicative Population transformer

-- WriterT has to be used instead of WeightedT,
-- since WeightedT uses StateT under the hood,
-- which requires a Monad (ListT m) constraint.

-- | A collection of weighted samples, or particles.
newtype PopulationT m a = PopulationT {getPopulationT :: WriterT (Log Double) (ListT m) a}
  deriving newtype (Functor, Applicative, Alternative)

runPopulationT :: PopulationT m a -> m [(a, Log Double)]
runPopulationT = runListT . runWriterT . getPopulationT

fromWeightedList :: m [(a, Log Double)] -> PopulationT m a
fromWeightedList = PopulationT . WriterT . listT
