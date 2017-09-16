
module Control.Monad.Bayes.Free (
  FreeSamplerT,
  hoist,
  interpret,
  withRandomness,
  withPartialRandomness
) where

-- import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Free

import Control.Monad.Bayes.Class

newtype SamF a = Random (Double -> a)

instance Functor SamF where
  fmap f (Random k) = Random (f . k)


newtype FreeSamplerT m a = FreeSamplerT (FreeT SamF m a)
  deriving(Functor,Applicative,Monad,MonadTrans)

runFreeSamplerT :: FreeSamplerT m a -> FreeT SamF m a
runFreeSamplerT (FreeSamplerT m) = m

instance Monad m => MonadFree SamF (FreeSamplerT m) where
  wrap = FreeSamplerT . wrap . fmap runFreeSamplerT

instance Monad m => MonadSample (FreeSamplerT m) where
  random = FreeSamplerT $ liftF (Random id)

hoist :: (Monad m) => (forall x. m x -> n x) -> FreeSamplerT m a -> FreeSamplerT n a
hoist f (FreeSamplerT m) = FreeSamplerT (hoistFreeT f m)

interpret :: MonadSample m => FreeSamplerT m a -> m a
interpret (FreeSamplerT m) = iterT f m where
  f (Random k) = random >>= k

withRandomness :: Monad m => [Double] -> FreeSamplerT m a -> m a
withRandomness randomness (FreeSamplerT m) = evalStateT (iterTM f m) randomness where
  f (Random k) = do
    xs <- get
    case xs of
      [] -> error "FreeSamplerT: the list of randomness was too short"
      y:ys -> put ys >> k y

withPartialRandomness :: MonadSample m => [Double] -> FreeSamplerT m a -> m (a, [Double])
withPartialRandomness randomness (FreeSamplerT m) =
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



-- newtype FreeSampler a = FreeSampler (Free SamF a)
--   deriving(Functor,Applicative,Monad)
--
-- instance MonadSample FreeSampler where
--   random = FreeSampler $ liftF (Random id)
--
-- interpret :: MonadSample m => FreeSampler a -> m a
-- interpret (FreeSampler m) = foldFree f m where
--   f (Random k) = fmap k random
--
-- withRandomness :: [Double] -> FreeSampler a -> a
-- withRandomness randomness (FreeSampler m) = run randomness m where
--   run _ (Pure x) = x
--   run (u:us) (Free (Random f)) = run us (f u)
--   run [] (Free _) = error "FreeSampler: the list of randomness was too short"
--
-- withPartialRandomness :: MonadSample m => [Double] -> FreeSampler a -> m (a, [Double])
-- withPartialRandomness randomness (FreeSampler m) = run randomness m where
--   run _ (Pure x) = pure (x, [])
--   run (u:us) (Free (Random f)) = do {(x, vs) <- run us (f u); return (x, u:vs)}
--   run [] (Free (Random f)) = do{v <- random; (x, vs) <- run [] (f v); return (x, v:vs)}
