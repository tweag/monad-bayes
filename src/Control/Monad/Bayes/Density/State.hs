{-# LANGUAGE DerivingStrategies #-}

module Control.Monad.Bayes.Density.State where
import Control.Monad.RWS
    ( MonadWriter(tell),
      MonadState(put, get),
      MonadTrans(..),
      RWST(RWST),
      evalRWST )
import Control.Monad.Bayes.Class (MonadSample (random))

newtype Density m a = Density (RWST () [Double] [Double] m a) deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance MonadSample m => MonadSample (Density m) where
    random = Density do 
        trace <- get
        case trace of
            [] -> do 
                r <- lift random
                tell [r]
                return r
            x:xs -> put xs >> tell [x] >> pure x
        
density :: Monad m => Density m b -> [Double] -> m (b, [Double])
density (Density m) = evalRWST m ()

example :: MonadSample m => m Double
example = do
    x <- random 
    y <- random
    return (x+y)