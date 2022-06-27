{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module BetaBin where

-- The beta-binomial model in latent variable and urn model representations.
-- The two formulations should be exactly equivalent, but only urn works with Dist.
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
  ( MonadInfer,
    MonadSample n (bernoulli, uniform),
    condition,
  )
import Control.Monad.State.Lazy (evalStateT, get, put)
import Pipes ((<-<))
import Pipes.Prelude qualified as P hiding (show)

-- | Beta-binomial model as an i.i.d. sequence conditionally on weight.
latent :: MonadSample n m => Int -> m [Bool]
latent n = do
  weight <- uniform 0 1
  replicateM n (bernoulli weight)

-- | Beta-binomial as a random process.
-- Equivalent to the above by De Finetti's theorem.
urn :: MonadSample n m => Int -> m [Bool]
urn n = flip evalStateT (1, 1) $ do
  replicateM n do
    (a, b) <- get
    let weight = a / (a + b)
    outcome <- bernoulli weight
    let (a', b') = if outcome then (a + 1, b) else (a, b + 1)
    put (a', b')
    return outcome

-- | Beta-binomial as a random process.
-- This time using the Pipes library, for a more pure functional style
urnP :: MonadSample n m => Int -> m [Bool]
urnP n = P.toListM $ P.take n <-< P.unfoldr toss (1, 1)
  where
    toss (a, b) = do
      let weight = a / (a + b)
      outcome <- bernoulli weight
      let (a', b') = if outcome then (a + 1, b) else (a, b + 1)
      return $ Right (outcome, (a', b'))

-- | A beta-binomial model where the first three states are True,True,False.
-- The resulting distribution is on the remaining outcomes.
cond :: MonadInfer n m => m [Bool] -> m [Bool]
cond d = do
  ~(first : second : third : rest) <- d
  condition first
  condition second
  condition (not third)
  return rest

-- | The final conditional model, abstracting the representation.
model :: MonadInfer n m => (Int -> m [Bool]) -> Int -> m Int
model repr n = fmap count $ cond $ repr (n + 3)
  where
    -- Post-processing by counting the number of True values.
    count :: [Bool] -> Int
    count = length . filter id
