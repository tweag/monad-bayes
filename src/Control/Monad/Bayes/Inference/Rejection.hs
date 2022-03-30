{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Bayes.Inference.Rejection where
import Control.Monad.Bayes.Class (MonadSample(bernoulli, random), MonadInfer, MonadCond (score))
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Bayes.Weighted ( Weighted, runWeighted )
import Numeric.Log (Log(ln))
import Control.Monad.Writer ( when, filterM, replicateM )

type ProbabilisticProgram a = (forall m. MonadInfer m => m a)

-- score statements must be in the unit interval
rejection :: MonadSample f => Int -> Weighted f b -> f [b]
rejection n m = (fmap . fmap) fst $ filterM (bernoulli . exp . ln . snd)
            =<< replicateM n (runWeighted m)


runRejection :: Show a => Int -> ProbabilisticProgram a -> IO ()
runRejection n m = do
    samples <- sampleIO $ rejection n m
    let ratio = fromIntegral (length samples) / fromIntegral n
    when (ratio < 0.1)
         (putStrLn $ "\nWARNING: Low ratio (" <> show ratio <> ") of samples surviving rejection sampling. This suggests that you may want to use a more advanced inference strategy like MCMC.\n")
    putStrLn $ "SAMPLES:" <> show samples <> "\n"


test = runRejection 1000 do
    x <- random
    score (100)
    return x