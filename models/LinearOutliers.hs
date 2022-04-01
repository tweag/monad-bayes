module LinearOutliers where

-- import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Bayes.Weighted
-- import Numeric.Log

regressionWithOutliers :: (MonadSample m, MonadCond m) =>
    [Double] -> [Double] -> m (Double, Double, Double, Double, [Bool])
regressionWithOutliers xs ys = do
    slope <- normal 0 2
    intercept <- normal 0 2
    noise <- gamma 1 1
    probOutlier <- uniform 0 1 

    let observation (x, y) = do
        is_outlier <- bernoulli probOutlier
        let (mu, std) = if is_outlier
            then (0, 10)
            else (x*slope + intercept, noise)
        factor $ normalPdf mu std y
        return is_outlier
    
    outliers <- mapM observation $ zip xs ys
    return (slope, intercept, noise, probOutlier, outliers)

t = sampleIO $ runWeighted $ regressionWithOutliers [1,2,3,4] [2,4,6,8]