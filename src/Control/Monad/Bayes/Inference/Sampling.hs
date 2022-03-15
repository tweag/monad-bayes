module Control.Monad.Bayes.Inference.Sampling where
import qualified Data.Vector as V
import Control.Monad.Bayes.Class (MonadSample(categorical))
import Control.Monad.Bayes.Enumerator (enumerate)

-- | This file provides tools to work with a set of samples

-- | The empirical distribution of a set of weighted samples
empirical :: Ord a => [(a, Double)] -> [(a, Double)]
empirical samples = enumerate $ do
    let (support, probs) = unzip samples
    i <- categorical $ V.fromList probs  
    return $ support !! i

-- | histogram of a set of samples
histogram :: Ord a => [a] -> [(a, Double)]
histogram = empirical . (`zip` repeat 1)