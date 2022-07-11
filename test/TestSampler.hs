module TestSampler where

import qualified Control.Foldl as Fold
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class (MonadSample (normal))
import Control.Monad.Bayes.Sampler (sampleSTfixed)

testMeanAndVariance :: Bool
testMeanAndVariance =
  abs (2 - sampleSTfixed (foldWith Fold.mean (normal 2 4))) < 0.001
    && abs (16 - sampleSTfixed (foldWith Fold.variance (normal 2 4))) < 0.1
  where
    foldWith f = fmap (Fold.fold f) . replicateM 100000
