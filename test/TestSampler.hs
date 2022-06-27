module TestSampler where

import qualified Control.Foldl as Fold
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class (MonadSample n (normal))
import Control.Monad.Bayes.Sampler (sampleSTfixed)

testMeanAndVariance :: Bool
testMeanAndVariance =
  2 - sampleSTfixed (foldWith Fold.mean (normal 2 4)) < 0.001
    && 4 - sampleSTfixed (foldWith Fold.variance (normal 2 4)) < 0.01
  where
    foldWith f = fmap (Fold.fold f) . replicateM 100000
