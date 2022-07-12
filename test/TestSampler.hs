module TestSampler where

import qualified Control.Foldl as Fold
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class (MonadSample (normal))
import Control.Monad.Bayes.Sampler (sampleSTfixed)
import Control.Monad.ST (ST, runST)

testMeanAndVariance :: Bool
testMeanAndVariance = isDiff
  where
    m = runST (sampleSTfixed (foldWith Fold.mean (normal 2 4)))
    v = runST (sampleSTfixed (foldWith Fold.variance (normal 2 4)))
    foldWith f = fmap (Fold.fold f) . replicateM 100000
    isDiff = abs (2 - m) < 0.01 && abs (16 - v) < 0.1
