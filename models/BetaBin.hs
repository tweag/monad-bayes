
module BetaBin where

-- The beta-binomial model in latent variable and urn model representations.
-- The two formulations should be exactly equivalent, but only urn works with Dist.

import Control.Monad.State.Lazy (get,put,evalStateT)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple

-- | Beta-binomial model as an i.i.d. sequence conditionally on weight.
latent :: MonadDist m => Int -> m [Bool]
latent n = do
  weight <- uniform 0 1
  let toss = bernoulli weight
  sequence $ replicate n $ toss

-- | Beta-binomial as a random process.
urn :: MonadDist m => Int -> m [Bool]
urn n = flip evalStateT (1,1) $ do
          let toss = do
                (a,b) <- get
                let weight = a / (a + b)
                outcome <- bernoulli weight
                let (a',b') = if outcome then (a+1,b) else (a,b+1)
                put (a',b')
                return outcome
          sequence $ replicate n $ toss

-- | Post-processing by counting the number of True values.
count :: [Bool] -> Int
count = length . filter id

-- | A beta-binomial model where the first three states are True,True,False.
-- The resulting distribution is on the remaining outcomes.
cond :: MonadBayes m => m [Bool] -> m [Bool]
cond d = do
  (first:second:third:rest) <- d
  condition (first  == True)
  condition (second == True)
  condition (third  == False)
  return rest

-- | The final conditional model, abstracting the representation.
model :: MonadBayes m => (Int -> m [Bool]) -> Int -> m Int
model repr n = fmap count $ cond $ repr (n+3)
