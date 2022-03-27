module BetaBin where

-- The beta-binomial model in latent variable and urn model representations.
-- The two formulations should be exactly equivalent, but only urn works with Dist.

import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
  ( MonadInfer,
    MonadSample (bernoulli, uniform),
    condition,
  )
import Control.Monad.State.Lazy (evalStateT, get, put)
import qualified Pipes.Prelude as P hiding (show)
import Pipes ((<-<))
import Control.Monad.Bayes.Enumerator (enumerate)
import Data.AEq ((~==))

-- | Beta-binomial model as an i.i.d. sequence conditionally on weight.
latent :: MonadSample m => Int -> m [Bool]
latent n = do
  weight <- uniform 0 1
  let toss = bernoulli weight
  replicateM n toss

-- | Beta-binomial as a random process. 
-- Equivalent to the above by De Finetti's theorem.
urn :: MonadSample m => Int -> m [Bool]
urn n = flip evalStateT (1, 1) $ do
  let toss = do
        (a, b) <- get
        let weight = a / (a + b)
        outcome <- bernoulli weight
        let (a', b') = if outcome then (a + 1, b) else (a, b + 1)
        put (a', b')
        return outcome
  replicateM n toss

-- | Beta-binomial as a random process. 
-- This time using the Pipes library, for a more pure functional style
urnP :: MonadSample m => Int -> m [Bool]
urnP n = P.toListM $ P.take n <-< P.unfoldr toss (1,1)
  where toss (a,b) = do
        let weight = a / (a + b)
        outcome <- bernoulli weight
        let (a', b') = if outcome then (a + 1, b) else (a, b + 1)
        return $ Right (outcome, (a', b'))

-- | Post-processing by counting the number of True values.
count :: [Bool] -> Int
count = length . filter id

-- | A beta-binomial model where the first three states are True,True,False.
-- The resulting distribution is on the remaining outcomes.
cond :: MonadInfer m => m [Bool] -> m [Bool]
cond d = do
  ~(first : second : third : rest) <- d
  condition first
  condition second
  condition (not third)
  return rest

-- | The final conditional model, abstracting the representation.
model :: MonadInfer m => (Int -> m [Bool]) -> Int -> m Int
model repr n = fmap count $ cond $ repr (n + 3)


test :: Int -> Bool
test n = enumerate (urn n) ~== enumerate (urnP n)

-- >>> test 10
-- True


-- >>> enumerate $ urnP 3
-- [([False,False,False],0.2500000000000001),([True,True,True],0.25),([False,False,True],8.333333333333337e-2),([False,True,False],8.333333333333337e-2),([False,True,True],8.333333333333337e-2),([True,False,False],8.333333333333337e-2),([True,False,True],8.333333333333337e-2),([True,True,False],8.333333333333337e-2)]
