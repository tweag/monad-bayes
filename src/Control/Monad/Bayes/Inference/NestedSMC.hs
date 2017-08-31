{-|
Module      : Control.Monad.Bayes.Inference.NestedSMC
Description : Nested Sequential Monte Carlo
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Inference.NestedSMC (
  innerLevel,
  nestedSMC
)  where

import Control.Monad.Trans (lift)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference

type S = Sequential
hoistS :: (forall x. m x -> m x) -> S m a -> S m a
hoistS = Control.Monad.Bayes.Sequential.hoistFirst

type P = Population
hoistP :: (Monad m, Monad n)
       => (forall x. m x -> n x) -> P m a -> P n a
hoistP = Control.Monad.Bayes.Population.hoist

hoistSP :: (Monad m)
       => (forall x. m x -> m x) -> S (P m) a -> S (P m) a
hoistSP f = hoistS (hoistP f)

-- | A tag to indicate a fragment on which inner SMC is to be run.
-- Should be applied to every full fragment before it's included
-- in the main program.
innerLevel :: Monad m
           => S (P m) a
           -> S (P (S (P m))) a
innerLevel m = do
  x <- lift (lift m)
  suspend
  return x

composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

-- | Nested Sequential Monte Carlo algorithm (Naesseth et al. 2015).
nestedSMC :: MonadSample m
          => Int -- ^ number of steps in the outer SMC
          -> Int -- ^ number of particles in the outer SMC
          -> Int -- ^ number of steps in the inner SMC
          -> Int -- ^ number of particles in the inner SMC
          -> S (P (S (P m))) a
          -> P m a
nestedSMC kOuter nOuter kInner nInner =
  strip . composeCopies kOuter outerStep . hoistS (spawn nOuter >>) where
    strip = flatten . hoistP finish . finish
    outerStep = advance . hoistS resample . innerSMC
    innerSMC = hoistSP (weigh . proper . smcMultinomial kInner nInner) where
      weigh d = lift $ do
        -- The 'do' block runs in the 'P m' monad.
        -- This is because we want to accumulate weight but not
        -- introduce a suspension.
        (x,w) <- lift d
        factor w
        return x
