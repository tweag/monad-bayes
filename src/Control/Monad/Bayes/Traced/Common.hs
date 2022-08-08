-- |
-- Module      : Control.Monad.Bayes.Traced.Common
-- Description : Numeric code for Trace MCMC
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Common where

-- ( Trace,
--   singleton,
--   output,
--   scored,
--   bind,
--   mhTrans,
--   mhTrans',
--   burnIn,
-- )

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Free as FreeSampler
  ( FreeSampler,
    hoist,
    withPartialRandomness,
    withRandomness,
  )
import Control.Monad.Bayes.Weighted as Weighted
  ( Weighted,
    hoist,
    weighted,
  )
import Control.Monad.Trans.Writer (WriterT (WriterT, runWriterT))
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Vector as V
import Numeric.AD
import Numeric.AD.Mode.Reverse
import Numeric.Log (Log (Exp), ln)
import Statistics.Distribution.DiscreteUniform (discreteUniformAB)

-- | Collection of random variables sampled during the program's execution.
data Trace n a = Trace
  { -- | Sequence of random variables sampled during the program's execution.
    variables :: [n],
    --
    output :: a,
    -- | The probability of observing this particular sequence.
    density :: Log n
  }

instance Functor (Trace n) where
  fmap f t = t {output = f (output t)}

instance RealFloat n => Applicative (Trace n) where
  pure x = Trace {variables = [], output = x, density = 1}
  tf <*> tx =
    Trace
      { variables = variables tf ++ variables tx,
        output = output tf (output tx),
        density = density tf * density tx
      }

instance RealFloat n => Monad (Trace n) where
  t >>= f =
    let t' = f (output t)
     in t' {variables = variables t ++ variables t', density = density t * density t'}

singleton :: RealFloat a => a -> Trace a a
singleton u = Trace {variables = [u], output = u, density = 1}

scored :: Log n -> Trace n ()
scored w = Trace {variables = [], output = (), density = w}

bind :: (Monad m, RealFloat n) => m (Trace n a) -> (a -> m (Trace n b)) -> m (Trace n b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  return $ t2 {variables = variables t1 ++ variables t2, density = density t1 * density t2}

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTrans :: (MonadSample n m, RealFloat n) => Weighted (FreeSampler m) n a -> Trace n a -> m n (Trace n a)
mhTrans m t@Trace {variables = us, density = p} = do
  let n = length us
  us' <- do
    i <- categorical (V.fromList $ replicate n 1) -- discrete $ discreteUniformAB 0 (n - 1)
    u' <- randomGeneric
    case splitAt i us of
      (xs, _ : ys) -> return $ xs ++ (u' : ys)
      _ -> error "impossible"
  ((b, q), vs) <- undefined -- runWriterT $ weighted $ Weighted.hoist (WriterT . withPartialRandomness us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  return $ if accept then Trace vs b q else t

-- | A variant of 'mhTrans' with an external sampling monad.
mhTrans' ::
  (MonadSample n m, RealFloat n) =>
  Weighted (FreeSampler IdentityN) n a ->
  Trace n a ->
  m n (Trace n a)
mhTrans' m = mhTrans (Weighted.hoist (FreeSampler.hoist (return . runIdentityN)) m)

-- | burn in an MCMC chain for n steps (which amounts to dropping samples of the end of the list)
burnIn :: Functor m => Int -> m [a] -> m [a]
burnIn n = fmap dropEnd
  where
    dropEnd ls = let len = length ls in take (len - n) ls



pdf ::
  (RealFloat n, Show n) =>
  Weighted (FreeSampler IdentityN) n a ->
  [n] ->
  n
pdf model rand =
  ln . exp . snd . runIdentityN . withRandomness rand $ weighted model

 -- :: RealFloat n => Weighted (FreeSampler IdentityN) (Reverse s n) Bool))

-- d :: (forall m n a. (RealFloat n, MonadInfer n m) => m n a) -> [n] -> n
-- d m =  (runWeighted m :: (MonadSample n m, RealFloat n) => Weighted m n a -> m n (a, Log n))
