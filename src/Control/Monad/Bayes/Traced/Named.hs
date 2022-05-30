{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Static
-- Description : Distributions on execution traces of full programs
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Named where

import Control.Applicative (liftA2)
import Control.Monad.Bayes.Class
    ( MonadSample(random, bernoulli), MonadInfer, MonadCond(..) )
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)
import Data.Map ( fromList, Map, empty )
import Data.Text (Text)
import Numeric.Log ( Log(ln) )
import Control.Monad.Bayes.Free as FreeSampler
    ( withPartialRandomnessCM, FreeSampler )
import Control.Monad.Bayes.Weighted as Weighted
    ( Weighted, runWeighted, hoist )
import Control.Monad.Trans.Writer ( WriterT(WriterT, runWriterT) )
import Control.Monad.State
    ( MonadState(get), StateT, MonadTrans(..), evalStateT )
import Data.Text qualified as T
import Control.Monad.State.Class ( modify )
import Data.Map qualified as M
import Debug.Trace (traceM)

-- | A tracing monad where only a subset of random choices are traced.
-- The random choices that are not to be traced should be lifted from the
-- transformed monad.
data Traced m a = Traced
  { model :: Weighted (FreeSampler m) a,
    traceDist :: m (ChoiceMap a)
  }

-- | Collection of random variables sampled during the program's execution.
data ChoiceMap a = ChoiceMap
  { -- | Named variables in the execution trace
    cm :: Map [Text] Double,
    variables :: [Double],
    -- | the output of the program
    output :: a,
    -- | The probability of observing this particular sequence.
    density :: Log Double
  } deriving Show
instance Functor ChoiceMap where
  fmap f t = t {output = f (output t)}
instance Applicative ChoiceMap where
  pure x = ChoiceMap {cm = empty, output = x, density = 1, variables = []}
  tf <*> tx =
    ChoiceMap
      { cm = cm tf <> cm tx,
        variables = variables tf <> variables tx,
        output = output tf (output tx),
        density = density tf * density tx
      }
instance Monad ChoiceMap where
  t >>= f =
    let t' = f (output t)
     in t' {cm = cm t <> cm t', variables = variables t ++ variables t', density = density t * density t'}

singleton :: Maybe [Text] -> Double -> ChoiceMap Double
singleton (Just v) u = ChoiceMap {cm = fromList [(v, u)], output = u, density = 1, variables = [u]}
singleton Nothing u = ChoiceMap {cm = empty, output = u, density = 1, variables = [u]}

scored :: Log Double -> ChoiceMap ()
scored w = ChoiceMap {cm = empty, output = (), density = w, variables = []}

bind :: Monad m => m (ChoiceMap a) -> (a -> m (ChoiceMap b)) -> m (ChoiceMap b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  return $ t2 {cm = cm t1 <> cm t2, variables = variables t1 <> variables t2, density = density t1 * density t2}
instance Monad m => Functor (Traced m) where
  fmap f (Traced m d) = Traced (fmap f m) (fmap (fmap f) d)

instance Monad m => Applicative (Traced m) where
  pure x = Traced (pure x) (pure (pure x))
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) (liftA2 (<*>) df dx)

instance Monad m => Monad (Traced m) where
  (Traced mx dx) >>= f = Traced my dy
    where
      my = mx >>= model . f
      dy = dx `bind` (traceDist . f)
instance MonadTrans Traced where
  lift m = Traced (lift $ lift m) (fmap pure m)

instance MonadSample m => MonadSample (Traced (StateT [Text] m)) where
  random = Traced random $ do
      v <- get
      singleton (case v of [] -> Nothing; x -> Just x) <$> random
instance MonadCond m => MonadCond (Traced m) where
  score w = Traced (score w) (score w >> pure (scored w))

instance MonadInfer m => MonadInfer (Traced (StateT [Text] m))

hoistT :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistT f (Traced m d) = Traced m (f d)

-- | Discard the trace and supporting infrastructure.
marginal :: Monad m => Traced m a -> m a
marginal (Traced _ d) = fmap output d

type Proposal m = M.Map [Text] Double -> m ( M.Map [Text] Double)

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: MonadSample m => Proposal m -> Traced (StateT [Text] m) a -> Traced (StateT [Text] m) a
mhStep prop (Traced m d) = Traced m d'
  where
    d' = d >>= lift . mhTrans prop m

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTrans :: MonadSample m =>
  Proposal m ->
  Weighted (FreeSampler (StateT [Text] m)) a -> ChoiceMap a -> m (ChoiceMap a)
mhTrans m p t = fst <$> mhTransWithBool m p t

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTransWithBool :: MonadSample m =>
  Proposal m ->
  Weighted (FreeSampler (StateT [Text] m)) a -> ChoiceMap a -> m (ChoiceMap a, Bool)
mhTransWithBool prop m t@ChoiceMap {cm = us, variables = allUs, density = p} = do
  us' <- prop us
  ((b, q), vs) <- runWriterT $ runWeighted $ Weighted.hoist (WriterT .  withPartialRandomnessCM us') m
  -- let qprob = q * fromIntegral (length allUs)
  --     ratio = if qprob == 0
  --     then 0
  --     else exp . ln $ min 1 (qprob / (p * fromIntegral (length vs)))
  let ratio = (exp . ln) $ min 1 (q * fromIntegral (length allUs) / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio --  (if isNaN ratio then error "ratio is Nan" else ratio) 
  traceM $ show ratio
  traceM $ show accept
  traceM $ show us
  traceM $ show us'
  return $ if accept then (ChoiceMap us' vs b q, accept) else (t, accept)

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps.
mh :: MonadSample m =>
  Proposal m -> Int -> Traced (StateT [Text] m) a -> m [a]
mh prop n (Traced m d) =

  (`evalStateT` []) $ fmap (map output . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) d
      | otherwise = do
        (x :| xs) <- f (k -1)
        y <- lift $ mhTrans prop m x
        return (y :| x : xs)

traced :: (MonadState [Text] m, Monad (t m), MonadTrans t) => Text -> t m b -> t m b
traced name program = lift (modify (<> [name])) >> program <* lift (modify init)