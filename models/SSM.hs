
{-# LANGUAGE
 RankNTypes,
 TypeFamilies
 #-}

module SSM where

import qualified Data.Vector as Vector
import Control.Monad.Trans
import Control.Monad.Trans.Identity

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Conditional

type Time = Int
data SSM m a = SSM {generative :: Time -> m (Vector.Vector (a, CustomReal m)),
                    posterior  :: Vector.Vector (CustomReal m)
                               -> m (Vector.Vector a)}

type Model a = forall m. MonadBayes m => m a

ssm :: MonadBayes m
    => m a -> (Time -> a -> m a) -> (Time -> a -> forall t. (MonadTrans t, MonadBayes (t m), CustomReal m ~ CustomReal (t m)) => t m (CustomReal m))
    -> SSM m a
ssm initial trans obs = SSM gen post where

  gen t = runIdentityT $ do
    let simulate 0 _ acc = return acc
        simulate n x acc = do
          x' <- lift $ trans (t-n) x
          y' <-        obs   (t-n) x'
          simulate (n-1) x' ((x',y'):acc)
    x0 <- lift $ initial
    ps <- simulate t x0 []
    return $ Vector.fromList $ reverse ps

  post ys = flip unsafeConditional (fromLists (Vector.toList ys,[])) $ do
    let t = Vector.length ys
        simulate 0 _ acc = return acc
        simulate n x acc = do
          x' <- lift $ trans (t-n) x
          y' <-        obs   (t-n) x'
          simulate (n-1) x' (x':acc)
    x0 <- lift $ initial
    ps <- simulate t x0 []
    return $ Vector.fromList $ reverse ps

type Mean r = r
type StdDev r = r
type Linear r = (r,r)

linearGaussian :: (MonadBayes m , CustomReal m ~ r)
               => (Mean r, StdDev r) -> (Linear r, StdDev r) -> StdDev r
               -> SSM m r
linearGaussian initial ((a,b),diff) noise =
  ssm (uncurry normal initial) (const (\x -> normal (a*x + b) diff))
      (const (\x -> normal x noise))
