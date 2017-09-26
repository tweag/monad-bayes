
module Control.Monad.Bayes.Free (
  FreeSampler,
  hoist,
  interpret,
  withRandomness,
  withPartialRandomness,
  pullWeight
) where

import Data.Bifunctor (second)

import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Free

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted hiding (hoist, pullWeight)

newtype SamF a = Random (Double -> a)

instance Functor SamF where
  fmap f (Random k) = Random (f . k)


newtype FreeSampler m a = FreeSampler (FreeT SamF m a)
  deriving(Functor,Applicative,Monad,MonadTrans)

runFreeSampler :: FreeSampler m a -> FreeT SamF m a
runFreeSampler (FreeSampler m) = m

instance Monad m => MonadFree SamF (FreeSampler m) where
  wrap = FreeSampler . wrap . fmap runFreeSampler

instance Monad m => MonadSample (FreeSampler m) where
  random = FreeSampler $ liftF (Random id)

hoist :: (Monad m) => (forall x. m x -> n x) -> FreeSampler m a -> FreeSampler n a
hoist f (FreeSampler m) = FreeSampler (hoistFreeT f m)

interpret :: MonadSample m => FreeSampler m a -> m a
interpret (FreeSampler m) = iterT f m where
  f (Random k) = random >>= k

withRandomness :: Monad m => [Double] -> FreeSampler m a -> m a
withRandomness randomness (FreeSampler m) = evalStateT (iterTM f m) randomness where
  f (Random k) = do
    xs <- get
    case xs of
      [] -> error "FreeSampler: the list of randomness was too short"
      y:ys -> put ys >> k y

withPartialRandomness :: MonadSample m => [Double] -> FreeSampler m a -> m (a, [Double])
withPartialRandomness randomness (FreeSampler m) =
  runWriterT $ evalStateT (iterTM f $ hoistFreeT lift m) randomness where
    f (Random k) = do
      -- This block runs in StateT [Double] (WriterT [Double]) m.
      -- StateT propagates consumed randomness while WriterT records
      -- randomness used, whether old or new.
      xs <- get
      x <- case xs of
            [] -> random
            y:ys -> put ys >> return y
      tell [x]
      k x

pullWeight :: Monad m => FreeSampler (Weighted m) a -> Weighted (FreeSampler m) a
pullWeight (FreeSampler m) = withWeight $ FreeSampler $ f m where
  f n = FreeT $ do
    (t, p) <- runWeighted $ runFreeT n
    return $ case t of
      Pure x -> Pure (x,p)
      Free s -> Free $ fmap (fmap (second (*p)) . f) s
