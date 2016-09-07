module TestDist where

import Data.AEq

import Control.Monad.Bayes.LogDomain (LogDomain, fromLogDomain)
import qualified Control.Monad.Bayes.Dist as Dist
import Control.Monad.Bayes.Class
import Sprinkler

enumerate :: Ord a => Dist.Dist Double a -> [(a,Double)]
enumerate = Dist.enumerate

evidence :: Dist.Dist Double a -> LogDomain Double
evidence = Dist.evidence

expectation :: (a -> Double) -> Dist.Dist Double a -> Double
expectation = Dist.expectation

unnorm :: MonadDist m => m Int
unnorm = discrete [0.5,0.8]

passed1 = fromLogDomain (evidence unnorm) ~== 1

agg :: MonadDist m => m Int
agg = do
  x <- uniformD [0,1]
  y <- uniformD [2,1]
  return (x+y)

passed2 = enumerate agg ~== [(1,0.25), (2,0.5), (3,0.25)]

passed3 = enumerate Sprinkler.hard ~== enumerate Sprinkler.soft

passed4 =
 (expectation (^ 2) (categorical [(1, 0.5), (2, 0.5)])) ~== 2.5
