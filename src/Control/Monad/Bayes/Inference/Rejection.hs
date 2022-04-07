{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Bayes.Inference.Rejection where
import Control.Monad.Bayes.Class (MonadSample(bernoulli, random), MonadInfer, MonadCond (score))
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Bayes.Weighted ( Weighted, runWeighted )
import Numeric.Log (Log(ln))
import Control.Monad.Writer ( when, filterM, replicateM )
import Control.Monad.Bayes.Enumerator (empirical, bin)

-- score statements must be in the unit interval
rejection :: MonadSample m => Int -> Weighted m b -> m [b]
rejection n m = (fmap . fmap) fst $ filterM (bernoulli . exp . ln . snd)
            =<< replicateM n (runWeighted m)

runRejection :: (Show a, Ord a) => Int -> (forall m. MonadInfer m => m a) -> IO ()
runRejection n m = do
    samples <- sampleIO $ rejection n m
    -- let foo = empirical (fmap (,1) samples)
    let ratio = fromIntegral (length samples) / fromIntegral n
    when (ratio < 0.1)
         (putStrLn $ "\nWARNING: Low ratio (" <> show ratio <> ") of samples surviving rejection sampling. This suggests that you may want to use a more advanced inference strategy like MCMC.\n")
    putStrLn $ "SAMPLES:" <> show samples <> "\n"
    -- print foo


test = runRejection 1000 do
    x <- random
    score 1
    return x