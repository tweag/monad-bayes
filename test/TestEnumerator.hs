module TestEnumerator where

import Data.AEq
import qualified Data.Vector as V

import Numeric.Log
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Class
import Sprinkler

unnorm :: MonadSample m => m Int
unnorm = categorical $ V.fromList [0.5,0.8]

passed1 :: Bool
passed1 = (exp . ln) (evidence unnorm) ~== 1

agg :: MonadSample m => m Int
agg = do
  x <- uniformD [0,1]
  y <- uniformD [2,1]
  return (x+y)

passed2 :: Bool
passed2 = enumerate agg ~== [(1,0.25), (2,0.5), (3,0.25)]

passed3 :: Bool
passed3 = enumerate Sprinkler.hard ~== enumerate Sprinkler.soft

passed4 :: Bool
passed4 =
 expectation (^ (2 :: Int)) (fmap (fromIntegral . (+1)) $ categorical $ V.fromList [0.5, 0.5]) ~== 2.5
