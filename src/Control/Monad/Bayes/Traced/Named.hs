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
module Control.Monad.Bayes.Traced.Named
  -- ( Traced,
  --   hoistT,
  --   marginal,
  --   mhStep,
  --   mh,
  -- )
where

import Control.Applicative (liftA2)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Free (FreeSampler)
-- import Control.Monad.Bayes.Traced.Common (mhTrans)
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Trans (MonadTrans (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)
import Data.Map hiding (singleton, map)
import Data.Text (Text)
import Numeric.Log

import Control.Monad.Bayes.Free as FreeSampler
import Control.Monad.Bayes.Weighted as Weighted
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Statistics.Distribution.DiscreteUniform (discreteUniformAB)
import Control.Monad.State
import Control.Monad.Bayes.Sampler (sampleIO, SamplerIO)
import Debug.Trace (trace)
import Lens.Micro ((^.), ix)
import Lens.Micro.GHC
import Data.Monoid (Ap (getAp))
import qualified Data.Text as T

-- | A tracing monad where only a subset of random choices are traced.
--
-- The random choices that are not to be traced should be lifted from the
-- transformed monad.
data Traced m a = Traced
  { model :: Weighted (FreeSampler m) a,
    traceDist :: m (ChoiceMap a)
  }

-- | Collection of random variables sampled during the program's execution.
data ChoiceMap a = ChoiceMap
  { -- | Named variables in the execution trace
    cm :: Map Text Double,
    variables :: [Double],
    -- |
    output :: a,
    -- | The probability of observing this particular sequence.
    density :: Log Double
  }

instance Functor ChoiceMap where
  fmap f t = t {output = f (output t)}

instance Applicative ChoiceMap where
  pure x = ChoiceMap {cm = empty, output = x, density = 1, variables = []}
  tf <*> tx =
    ChoiceMap
      { cm = cm tf <> cm tx,
        variables = variables tf ++ variables tx,
        output = output tf (output tx),
        density = density tf * density tx
      }

instance Monad ChoiceMap where
  t >>= f =
    let t' = f (output t)
     in t' {cm = cm t <> cm t', variables = variables t ++ variables t', density = density t * density t'}

singleton :: Maybe Text -> Double -> ChoiceMap Double
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

-- instance MonadSample m => MonadSample (Traced m) where
--   random = Traced random (singleton <$> random)

instance MonadSample m => MonadSample (Traced (StateT Text m)) where
  random = Traced random $ do
      v <- get
      singleton (case v of "" -> Nothing; x -> Just x) <$> random

instance MonadCond m => MonadCond (Traced m) where
  score w = Traced (score w) (score w >> pure (scored w))

instance MonadInfer m => MonadInfer (Traced (StateT Text m))

hoistT :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistT f (Traced m d) = Traced m (f d)

-- | Discard the trace and supporting infrastructure.
marginal :: Monad m => Traced m a -> m a
marginal (Traced _ d) = fmap output d

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: MonadSample m => (Map Text Double -> m (Map Text Double)) -> Traced (StateT Text m) a -> Traced (StateT Text m) a
mhStep prop (Traced m d) = Traced m d'
  where
    d' = d >>= lift . mhTrans prop m

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTrans :: MonadSample m =>
  (Map Text Double -> m (Map Text Double)) ->
  Weighted (FreeSampler (StateT Text m)) a -> ChoiceMap a -> m (ChoiceMap a)
mhTrans prop m t@ChoiceMap {cm = us, variables = allUs, density = p} = do
  us' <- prop us
  ((b, q), vs) <- runWriterT $ runWeighted $ Weighted.hoist (WriterT .  withPartialRandomnessCM us') m
  let qprob = q * fromIntegral (length allUs)
      ratio = if qprob == 0
      then 0
      else exp . ln $ min 1 (qprob / (p * fromIntegral (length vs)))
  accept <- bernoulli (if isNaN ratio then error "ratio is Nan" else ratio) -- trace (show (q, p, us')) ratio
  return $ if accept then ChoiceMap us' vs b q else t
  -- trace ("|" <> show vs <> "|" <> show allUs <> "|") $ 

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps.
mh :: MonadSample m =>
  (Map Text Double -> m (Map Text Double)) -> Int -> Traced (StateT Text m) a -> StateT Text m [a]
mh prop n (Traced m d) =

  fmap (map output . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) d
      | otherwise = do
        (x :| xs) <- f (k -1)
        y <- lift $ mhTrans prop m x
        return (y :| x : xs)

example = fmap reverse $ sampleIO $ prior $ flip evalStateT "" $ mh
  prop2
  -- ((ix "x") (const $ random) >=> (ix "y") (const $ random))
  1000 ex2

prop2 k = do
  key <- uniformD ["x", "y"]
  (ix key) (const $ random) k

up :: Applicative m =>
  ( Double -> m Double) ->
  Map Text Double ->
    m (Map Text Double)
up = ix "x"

thing = sampleIO $ prior $ flip evalStateT "" $ fmap cm $ traceDist ex2

cmp = Data.Map.fromList [("x", 0.6), ("y", 0.6)]


ex2 :: (MonadTrans t, MonadState Text m, MonadSample (t m),
 MonadCond (t m)) =>
  t m (Double, Double)
ex2 = do
  x <-  traced "x" (random >> traced "i" (random >> traced "j" random))
  y <- traced "y" random
  condition (x > 0.7 && y > 0.7)
  z <- bernoulli 0.5
  return (x,y)

test1 = do
  choiceMap <- sampleIO $ prior $ flip evalStateT "" $ cm <$> traceDist ex2
  print $ keys choiceMap == ["x","xi","xij","y"]

  s <- sampleIO $ prior $ flip evalStateT "" $ mh (const $ return cmp) 3 ex2
  print s

traced :: (MonadState Text m, Monad (t m), MonadTrans t) => Text -> t m b -> t m b
traced name program = lift (modify (<> name)) >> program <* lift (modify T.init)