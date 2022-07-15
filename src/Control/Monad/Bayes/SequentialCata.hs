{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Bayes.SequentialCata where
import Control.Monad.Bayes.Free (FreeSampler)
import Control.Monad.Trans.Free (FreeT, FreeF (Free, Pure), liftF, MonadFree (wrap))
import Control.Monad.Reader
import Control.Monad.Bayes.Class 
import Data.Functor.Foldable (Recursive(cata, prepro, gprepro), gcata, distCata, distHisto, Base)
import Data.Functor.Compose (Compose(Compose))
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Numeric.Log (Log)
import Control.Monad.Identity (Identity(Identity, runIdentity))
import Control.Comonad (Comonad(extract, extend))
import Control.Comonad.Cofree
import Debug.Trace (traceM)
import Control.Monad.Bayes.Population (resampleMultinomial, runPopulation, spawn)

data SamF a = Random (Double -> a) | Score (Log Double) (() -> a) deriving Functor

type Initial m = FreeT SamF m

instance Monad m => MonadSample (Initial m) where
  random =  liftF (Random id)

instance Monad m => MonadCond (Initial m) where
  score d =  liftF $ Score d id

instance Monad m => MonadInfer (Initial m)

f :: MonadInfer m => (m a -> m a) -> Initial m a -> m a
f transform = gprepro distHisto distribute \case
    Compose m -> do
        m' <- m
        case m' of
          Pure a -> traceM "baz" >> return a
          Free ((Random (f))) -> extract . f =<< random -- do
            --   x <- random
            --   traceM "bar"
            --   let out = f x
            --   let (bar :< ( baz)) = out
            -- --   baz' <- baz

            --   bar
          Free (Score d f ) -> transform (score d >> extract (f ()))


tr :: MonadSample m => 
    Cofree (Compose m (FreeF SamF a)) (m a) 
    -> Cofree (Compose m (FreeF SamF a)) (m a) 
tr = extend \case 
    ma :< (Compose com) -> do
        undefined
    

distribute :: MonadInfer m => Base (FreeT SamF m a) c -> Base (FreeT SamF m a) c
distribute (Compose m) = Compose do 
    m' <- m
    case m' of 
      Pure a -> do
        --   traceM "foo"
          return (Pure a)
      Free sf@(Random _) -> do
        --   traceM "bar"
          return (Free sf)
      Free sf@(Score a b) -> do
        --   traceM "baz"
          return (Free sf)

a :: MonadInfer m => m Double
a = do
    x <- random
    condition (x > 0.4)
    y <- random
    -- y <- normal (0) 1
    -- z <- random
    -- z' <- random
    return ( x + y )

test = sampleIO $ runPopulation $ f resampleMultinomial (lift (spawn 2) >> a) 